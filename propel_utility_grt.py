from wb import *
import sys
import os
import grt
import mforms
import re
import pickle
import new
import warnings
import tempfile
import binascii

# ElementTree


__all__ = [
    "Comment",
    "dump",
    "Element", "ElementTree",
    "fromstring", "fromstringlist",
    "iselement", "iterparse",
    "parse", "ParseError",
    "PI", "ProcessingInstruction",
    "QName",
    "SubElement",
    "tostring", "tostringlist",
    "TreeBuilder",
    "VERSION",
    "XML",
    "XMLParser", "XMLTreeBuilder",
    ]

VERSION = "1.3.0"




class _SimpleElementPath(object):
    def find(self, element, tag, namespaces=None):
        for elem in element:
            if elem.tag == tag:
                return elem
        return None
    def findtext(self, element, tag, default=None, namespaces=None):
        elem = self.find(element, tag)
        if elem is None:
            return default
        return elem.text or ""
    def iterfind(self, element, tag, namespaces=None):
        if tag[:3] == ".//":
            for elem in element.iter(tag[3:]):
                yield elem
        for elem in element:
            if elem.tag == tag:
                yield elem
    def findall(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))

try:
    from ElementTree import ElementPath
except ImportError:
    ElementPath = _SimpleElementPath()


class ParseError(SyntaxError):
    pass



def iselement(element):
    return isinstance(element, Element) or hasattr(element, "tag")


class Element(object):


    tag = None


    attrib = None


    text = None


    tail = None # text after end tag, if any


    def __init__(self, tag, attrib={}, **extra):
        attrib = attrib.copy()
        attrib.update(extra)
        self.tag = tag
        self.attrib = attrib
        self._children = []

    def __repr__(self):
        return "<Element %s at 0x%x>" % (repr(self.tag), id(self))


    def makeelement(self, tag, attrib):
        return self.__class__(tag, attrib)


    def copy(self):
        elem = self.makeelement(self.tag, self.attrib)
        elem.text = self.text
        elem.tail = self.tail
        elem[:] = self
        return elem


    def __len__(self):
        return len(self._children)

    def __nonzero__(self):
        warnings.warn(
            "The behavior of this method will change in future versions.  "
            "Use specific 'len(elem)' or 'elem is not None' test instead.",
            FutureWarning, stacklevel=2
            )
        return len(self._children) != 0 # emulate old behaviour, for now


    def __getitem__(self, index):
        return self._children[index]


    def __setitem__(self, index, element):
        self._children[index] = element


    def __delitem__(self, index):
        del self._children[index]


    def append(self, element):
        self._children.append(element)


    def extend(self, elements):
        self._children.extend(elements)


    def insert(self, index, element):
        self._children.insert(index, element)


    def remove(self, element):
        self._children.remove(element)


    def getchildren(self):
        warnings.warn(
            "This method will be removed in future versions.  "
            "Use 'list(elem)' or iteration over elem instead.",
            DeprecationWarning, stacklevel=2
            )
        return self._children


    def find(self, path, namespaces=None):
        return ElementPath.find(self, path, namespaces)


    def findtext(self, path, default=None, namespaces=None):
        return ElementPath.findtext(self, path, default, namespaces)


    def findall(self, path, namespaces=None):
        return ElementPath.findall(self, path, namespaces)


    def iterfind(self, path, namespaces=None):
        return ElementPath.iterfind(self, path, namespaces)


    def clear(self):
        self.attrib.clear()
        self._children = []
        self.text = self.tail = None


    def get(self, key, default=None):
        return self.attrib.get(key, default)


    def set(self, key, value):
        self.attrib[key] = value


    def keys(self):
        return self.attrib.keys()


    def items(self):
        return self.attrib.items()


    def iter(self, tag=None):
        if tag == "*":
            tag = None
        if tag is None or self.tag == tag:
            yield self
        for e in self._children:
            for e in e.iter(tag):
                yield e

    def getiterator(self, tag=None):
        warnings.warn(
            "This method will be removed in future versions.  "
            "Use 'elem.iter()' or 'list(elem.iter())' instead.",
            PendingDeprecationWarning, stacklevel=2
        )
        return list(self.iter(tag))


    def itertext(self):
        tag = self.tag
        if not isinstance(tag, basestring) and tag is not None:
            return
        if self.text:
            yield self.text
        for e in self:
            for s in e.itertext():
                yield s
            if e.tail:
                yield e.tail

_Element = _ElementInterface = Element


def SubElement(parent, tag, attrib={}, **extra):
    attrib = attrib.copy()
    attrib.update(extra)
    element = parent.makeelement(tag, attrib)
    parent.append(element)
    return element


def Comment(text=None):
    element = Element(Comment)
    element.text = text
    return element


def ProcessingInstruction(target, text=None):
    element = Element(ProcessingInstruction)
    element.text = target
    if text:
        element.text = element.text + " " + text
    return element

PI = ProcessingInstruction


class QName(object):
    def __init__(self, text_or_uri, tag=None):
        if tag:
            text_or_uri = "{%s}%s" % (text_or_uri, tag)
        self.text = text_or_uri
    def __str__(self):
        return self.text
    def __hash__(self):
        return hash(self.text)
    def __cmp__(self, other):
        if isinstance(other, QName):
            return cmp(self.text, other.text)
        return cmp(self.text, other)



class ElementTree(object):

    def __init__(self, element=None, file=None):
        self._root = element # first node
        if file:
            self.parse(file)


    def getroot(self):
        return self._root


    def _setroot(self, element):
        self._root = element


    def parse(self, source, parser=None):
        if not hasattr(source, "read"):
            source = open(source, "rb")
        if not parser:
            parser = XMLParser(target=TreeBuilder())
        while 1:
            data = source.read(65536)
            if not data:
                break
            parser.feed(data)
        self._root = parser.close()
        return self._root


    def iter(self, tag=None):
        return self._root.iter(tag)

    def getiterator(self, tag=None):
        warnings.warn(
            "This method will be removed in future versions.  "
            "Use 'tree.iter()' or 'list(tree.iter())' instead.",
            PendingDeprecationWarning, stacklevel=2
        )
        return list(self.iter(tag))


    def find(self, path, namespaces=None):
        if path[:1] == "/":
            path = "." + path
            warnings.warn(
                "This search is broken in 1.3 and earlier, and will be "
                "fixed in a future version.  If you rely on the current "
                "behaviour, change it to %r" % path,
                FutureWarning, stacklevel=2
                )
        return self._root.find(path, namespaces)


    def findtext(self, path, default=None, namespaces=None):
        if path[:1] == "/":
            path = "." + path
            warnings.warn(
                "This search is broken in 1.3 and earlier, and will be "
                "fixed in a future version.  If you rely on the current "
                "behaviour, change it to %r" % path,
                FutureWarning, stacklevel=2
                )
        return self._root.findtext(path, default, namespaces)


    def findall(self, path, namespaces=None):
        if path[:1] == "/":
            path = "." + path
            warnings.warn(
                "This search is broken in 1.3 and earlier, and will be "
                "fixed in a future version.  If you rely on the current "
                "behaviour, change it to %r" % path,
                FutureWarning, stacklevel=2
                )
        return self._root.findall(path, namespaces)


    def iterfind(self, path, namespaces=None):
        if path[:1] == "/":
            path = "." + path
            warnings.warn(
                "This search is broken in 1.3 and earlier, and will be "
                "fixed in a future version.  If you rely on the current "
                "behaviour, change it to %r" % path,
                FutureWarning, stacklevel=2
                )
        return self._root.iterfind(path, namespaces)


    def write(self, file_or_filename,
              encoding=None,
              xml_declaration=None,
              default_namespace=None,
              method=None):
        if not method:
            method = "xml"
        elif method not in _serialize:
            raise ValueError("unknown method %r" % method)
        if hasattr(file_or_filename, "write"):
            file = file_or_filename
        else:
            file = open(file_or_filename, "wb")
        write = file.write
        if not encoding:
            if method == "c14n":
                encoding = "utf-8"
            else:
                encoding = "us-ascii"
        elif xml_declaration or (xml_declaration is None and
                                 encoding not in ("utf-8", "us-ascii")):
            if method == "xml":
                write("<?xml version='1.0' encoding='%s'?>\n" % encoding)
        if method == "text":
            _serialize_text(write, self._root, encoding)
        else:
            qnames, namespaces = _namespaces(
                self._root, encoding, default_namespace
                )
            serialize = _serialize[method]
            serialize(write, self._root, encoding, qnames, namespaces)
        if file_or_filename is not file:
            file.close()

    def write_c14n(self, file):
        return self.write(file, method="c14n")


def _namespaces(elem, encoding, default_namespace=None):

    qnames = {None: None}

    namespaces = {}
    if default_namespace:
        namespaces[default_namespace] = ""

    def encode(text):
        return text.encode(encoding)

    def add_qname(qname):
        try:
            if qname[:1] == "{":
                uri, tag = qname[1:].rsplit("}", 1)
                prefix = namespaces.get(uri)
                if prefix is None:
                    prefix = _namespace_map.get(uri)
                    if prefix is None:
                        prefix = "ns%d" % len(namespaces)
                    if prefix != "xml":
                        namespaces[uri] = prefix
                if prefix:
                    qnames[qname] = encode("%s:%s" % (prefix, tag))
                else:
                    qnames[qname] = encode(tag) # default element
            else:
                if default_namespace:
                    raise ValueError(
                        "cannot use non-qualified names with "
                        "default_namespace option"
                        )
                qnames[qname] = encode(qname)
        except TypeError:
            _raise_serialization_error(qname)

    try:
        iterate = elem.iter
    except AttributeError:
        iterate = elem.getiterator # cET compatibility
    for elem in iterate():
        tag = elem.tag
        if isinstance(tag, QName) and tag.text not in qnames:
            add_qname(tag.text)
        elif isinstance(tag, basestring):
            if tag not in qnames:
                add_qname(tag)
        elif tag is not None and tag is not Comment and tag is not PI:
            _raise_serialization_error(tag)
        for key, value in elem.items():
            if isinstance(key, QName):
                key = key.text
            if key not in qnames:
                add_qname(key)
            if isinstance(value, QName) and value.text not in qnames:
                add_qname(value.text)
        text = elem.text
        if isinstance(text, QName) and text.text not in qnames:
            add_qname(text.text)
    return qnames, namespaces

def _serialize_xml(write, elem, encoding, qnames, namespaces):
    tag = elem.tag
    text = elem.text
    if tag is Comment:
        write("<!--%s-->" % _encode(text, encoding))
    elif tag is ProcessingInstruction:
        write("<?%s?>" % _encode(text, encoding))
    else:
        tag = qnames[tag]
        if tag is None:
            if text:
                write(_escape_cdata(text, encoding))
            for e in elem:
                _serialize_xml(write, e, encoding, qnames, None)
        else:
            write("<" + tag)
            items = elem.items()
            if items or namespaces:
                if namespaces:
                    for v, k in sorted(namespaces.items(),
                                       key=lambda x: x[1]):  # sort on prefix
                        if k:
                            k = ":" + k
                        write(" xmlns%s=\"%s\"" % (
                            k.encode(encoding),
                            _escape_attrib(v, encoding)
                            ))
                for k, v in sorted(items):  # lexical order
                    if isinstance(k, QName):
                        k = k.text
                    if isinstance(v, QName):
                        v = qnames[v.text]
                    else:
                        v = _escape_attrib(v, encoding)
                    write(" %s=\"%s\"" % (qnames[k], v))
            if text or len(elem):
                write(">")
                if text:
                    write(_escape_cdata(text, encoding))
                for e in elem:
                    _serialize_xml(write, e, encoding, qnames, None)
                write("</" + tag + ">")
            else:
                write(" />")
    if elem.tail:
        write(_escape_cdata(elem.tail, encoding))

HTML_EMPTY = ("area", "base", "basefont", "br", "col", "frame", "hr",
              "img", "input", "isindex", "link", "meta" "param")

try:
    HTML_EMPTY = set(HTML_EMPTY)
except NameError:
    pass

def _serialize_html(write, elem, encoding, qnames, namespaces):
    tag = elem.tag
    text = elem.text
    if tag is Comment:
        write("<!--%s-->" % _escape_cdata(text, encoding))
    elif tag is ProcessingInstruction:
        write("<?%s?>" % _escape_cdata(text, encoding))
    else:
        tag = qnames[tag]
        if tag is None:
            if text:
                write(_escape_cdata(text, encoding))
            for e in elem:
                _serialize_html(write, e, encoding, qnames, None)
        else:
            write("<" + tag)
            items = elem.items()
            if items or namespaces:
                if namespaces:
                    for v, k in sorted(namespaces.items(),
                                       key=lambda x: x[1]):  # sort on prefix
                        if k:
                            k = ":" + k
                        write(" xmlns%s=\"%s\"" % (
                            k.encode(encoding),
                            _escape_attrib(v, encoding)
                            ))
                for k, v in sorted(items):  # lexical order
                    if isinstance(k, QName):
                        k = k.text
                    if isinstance(v, QName):
                        v = qnames[v.text]
                    else:
                        v = _escape_attrib_html(v, encoding)
                    write(" %s=\"%s\"" % (qnames[k], v))
            write(">")
            tag = tag.lower()
            if text:
                if tag == "script" or tag == "style":
                    write(_encode(text, encoding))
                else:
                    write(_escape_cdata(text, encoding))
            for e in elem:
                _serialize_html(write, e, encoding, qnames, None)
            if tag not in HTML_EMPTY:
                write("</" + tag + ">")
    if elem.tail:
        write(_escape_cdata(elem.tail, encoding))

def _serialize_text(write, elem, encoding):
    for part in elem.itertext():
        write(part.encode(encoding))
    if elem.tail:
        write(elem.tail.encode(encoding))

_serialize = {
    "xml": _serialize_xml,
    "html": _serialize_html,
    "text": _serialize_text,
}


def register_namespace(prefix, uri):
    if re.match("ns\d+$", prefix):
        raise ValueError("Prefix format reserved for internal use")
    for k, v in _namespace_map.items():
        if k == uri or v == prefix:
            del _namespace_map[k]
    _namespace_map[uri] = prefix

_namespace_map = {
    "http://www.w3.org/XML/1998/namespace": "xml",
    "http://www.w3.org/1999/xhtml": "html",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#": "rdf",
    "http://schemas.xmlsoap.org/wsdl/": "wsdl",
    "http://www.w3.org/2001/XMLSchema": "xs",
    "http://www.w3.org/2001/XMLSchema-instance": "xsi",
    "http://purl.org/dc/elements/1.1/": "dc",
}

def _raise_serialization_error(text):
    raise TypeError(
        "cannot serialize %r (type %s)" % (text, type(text).__name__)
        )

def _encode(text, encoding):
    try:
        return text.encode(encoding, "xmlcharrefreplace")
    except (TypeError, AttributeError):
        _raise_serialization_error(text)

def _escape_cdata(text, encoding):
    try:
        if "&" in text:
            text = text.replace("&", "&amp;")
        if "<" in text:
            text = text.replace("<", "&lt;")
        if ">" in text:
            text = text.replace(">", "&gt;")
        return text.encode(encoding, "xmlcharrefreplace")
    except (TypeError, AttributeError):
        _raise_serialization_error(text)

def _escape_attrib(text, encoding):
    try:
        if "&" in text:
            text = text.replace("&", "&amp;")
        if "<" in text:
            text = text.replace("<", "&lt;")
        if ">" in text:
            text = text.replace(">", "&gt;")
        if "\"" in text:
            text = text.replace("\"", "&quot;")
        if "\n" in text:
            text = text.replace("\n", "&#10;")
        return text.encode(encoding, "xmlcharrefreplace")
    except (TypeError, AttributeError):
        _raise_serialization_error(text)

def _escape_attrib_html(text, encoding):
    try:
        if "&" in text:
            text = text.replace("&", "&amp;")
        if ">" in text:
            text = text.replace(">", "&gt;")
        if "\"" in text:
            text = text.replace("\"", "&quot;")
        return text.encode(encoding, "xmlcharrefreplace")
    except (TypeError, AttributeError):
        _raise_serialization_error(text)



def tostring(element, encoding=None, method=None):
    class dummy:
        pass
    data = []
    file = dummy()
    file.write = data.append
    ElementTree(element).write(file, encoding, method=method)
    return "".join(data)


def tostringlist(element, encoding=None, method=None):
    class dummy:
        pass
    data = []
    file = dummy()
    file.write = data.append
    ElementTree(element).write(file, encoding, method=method)
    return data


def dump(elem):
    if not isinstance(elem, ElementTree):
        elem = ElementTree(elem)
    elem.write(sys.stdout)
    tail = elem.getroot().tail
    if not tail or tail[-1] != "\n":
        sys.stdout.write("\n")



def parse(source, parser=None):
    tree = ElementTree()
    tree.parse(source, parser)
    return tree


def iterparse(source, events=None, parser=None):
    if not hasattr(source, "read"):
        source = open(source, "rb")
    if not parser:
        parser = XMLParser(target=TreeBuilder())
    return _IterParseIterator(source, events, parser)

class _IterParseIterator(object):

    def __init__(self, source, events, parser):
        self._file = source
        self._events = []
        self._index = 0
        self.root = self._root = None
        self._parser = parser
        parser = self._parser._parser
        append = self._events.append
        if events is None:
            events = ["end"]
        for event in events:
            if event == "start":
                try:
                    parser.ordered_attributes = 1
                    parser.specified_attributes = 1
                    def handler(tag, attrib_in, event=event, append=append,
                                start=self._parser._start_list):
                        append((event, start(tag, attrib_in)))
                    parser.StartElementHandler = handler
                except AttributeError:
                    def handler(tag, attrib_in, event=event, append=append,
                                start=self._parser._start):
                        append((event, start(tag, attrib_in)))
                    parser.StartElementHandler = handler
            elif event == "end":
                def handler(tag, event=event, append=append,
                            end=self._parser._end):
                    append((event, end(tag)))
                parser.EndElementHandler = handler
            elif event == "start-ns":
                def handler(prefix, uri, event=event, append=append):
                    try:
                        uri = (uri or "").encode("ascii")
                    except UnicodeError:
                        pass
                    append((event, (prefix or "", uri or "")))
                parser.StartNamespaceDeclHandler = handler
            elif event == "end-ns":
                def handler(prefix, event=event, append=append):
                    append((event, None))
                parser.EndNamespaceDeclHandler = handler
            else:
                raise ValueError("unknown event %r" % event)

    def next(self):
        while 1:
            try:
                item = self._events[self._index]
            except IndexError:
                if self._parser is None:
                    self.root = self._root
                    raise StopIteration
                del self._events[:]
                self._index = 0
                data = self._file.read(16384)
                if data:
                    self._parser.feed(data)
                else:
                    self._root = self._parser.close()
                    self._parser = None
            else:
                self._index = self._index + 1
                return item

    def __iter__(self):
        return self


def XML(text, parser=None):
    if not parser:
        parser = XMLParser(target=TreeBuilder())
    parser.feed(text)
    return parser.close()


def XMLID(text, parser=None):
    if not parser:
        parser = XMLParser(target=TreeBuilder())
    parser.feed(text)
    tree = parser.close()
    ids = {}
    for elem in tree.iter():
        id = elem.get("id")
        if id:
            ids[id] = elem
    return tree, ids




def fromstringlist(sequence, parser=None):
    if not parser:
        parser = XMLParser(target=TreeBuilder())
    for text in sequence:
        parser.feed(text)
    return parser.close()



class TreeBuilder(object):

    def __init__(self, element_factory=None):
        self._data = [] # data collector
        self._elem = [] # element stack
        self._last = None # last element
        self._tail = None # true if we're after an end tag
        if element_factory is None:
            element_factory = Element
        self._factory = element_factory


    def close(self):
        assert len(self._elem) == 0, "missing end tags"
        assert self._last is not None, "missing toplevel element"
        return self._last

    def _flush(self):
        if self._data:
            if self._last is not None:
                text = "".join(self._data)
                if self._tail:
                    assert self._last.tail is None, "internal error (tail)"
                    self._last.tail = text
                else:
                    assert self._last.text is None, "internal error (text)"
                    self._last.text = text
            self._data = []


    def data(self, data):
        self._data.append(data)


    def start(self, tag, attrs):
        self._flush()
        self._last = elem = self._factory(tag, attrs)
        if self._elem:
            self._elem[-1].append(elem)
        self._elem.append(elem)
        self._tail = 0
        return elem


    def end(self, tag):
        self._flush()
        self._last = self._elem.pop()
        assert self._last.tag == tag,\
               "end tag mismatch (expected %s, got %s)" % (
                   self._last.tag, tag)
        self._tail = 1
        return self._last


class XMLParser(object):

    def __init__(self, html=0, target=None, encoding=None):
        try:
            from xml.parsers import expat
        except ImportError:
            try:
                import pyexpat as expat
            except ImportError:
                raise ImportError(
                    "No module named expat; use SimpleXMLTreeBuilder instead"
                    )
        parser = expat.ParserCreate(encoding, "}")
        if target is None:
            target = TreeBuilder()
        self.parser = self._parser = parser
        self.target = self._target = target
        self._error = expat.error
        self._names = {} # name memo cache
        parser.DefaultHandlerExpand = self._default
        parser.StartElementHandler = self._start
        parser.EndElementHandler = self._end
        parser.CharacterDataHandler = self._data
        parser.CommentHandler = self._comment
        parser.ProcessingInstructionHandler = self._pi
        try:
            self._parser.buffer_text = 1
        except AttributeError:
            pass
        try:
            self._parser.ordered_attributes = 1
            self._parser.specified_attributes = 1
            parser.StartElementHandler = self._start_list
        except AttributeError:
            pass
        self._doctype = None
        self.entity = {}
        try:
            self.version = "Expat %d.%d.%d" % expat.version_info
        except AttributeError:
            pass # unknown

    def _raiseerror(self, value):
        err = ParseError(value)
        err.code = value.code
        err.position = value.lineno, value.offset
        raise err

    def _fixtext(self, text):
        try:
            return text.encode("ascii")
        except UnicodeError:
            return text

    def _fixname(self, key):
        try:
            name = self._names[key]
        except KeyError:
            name = key
            if "}" in name:
                name = "{" + name
            self._names[key] = name = self._fixtext(name)
        return name

    def _start(self, tag, attrib_in):
        fixname = self._fixname
        fixtext = self._fixtext
        tag = fixname(tag)
        attrib = {}
        for key, value in attrib_in.items():
            attrib[fixname(key)] = fixtext(value)
        return self.target.start(tag, attrib)

    def _start_list(self, tag, attrib_in):
        fixname = self._fixname
        fixtext = self._fixtext
        tag = fixname(tag)
        attrib = {}
        if attrib_in:
            for i in range(0, len(attrib_in), 2):
                attrib[fixname(attrib_in[i])] = fixtext(attrib_in[i+1])
        return self.target.start(tag, attrib)

    def _data(self, text):
        return self.target.data(self._fixtext(text))

    def _end(self, tag):
        return self.target.end(self._fixname(tag))

    def _comment(self, data):
        try:
            comment = self.target.comment
        except AttributeError:
            pass
        else:
            return comment(self._fixtext(data))

    def _pi(self, target, data):
        try:
            pi = self.target.pi
        except AttributeError:
            pass
        else:
            return pi(self._fixtext(target), self._fixtext(data))

    def _default(self, text):
        prefix = text[:1]
        if prefix == "&":
            try:
                self.target.data(self.entity[text[1:-1]])
            except KeyError:
                from xml.parsers import expat
                err = expat.error(
                    "undefined entity %s: line %d, column %d" %
                    (text, self._parser.ErrorLineNumber,
                    self._parser.ErrorColumnNumber)
                    )
                err.code = 11 # XML_ERROR_UNDEFINED_ENTITY
                err.lineno = self._parser.ErrorLineNumber
                err.offset = self._parser.ErrorColumnNumber
                raise err
        elif prefix == "<" and text[:9] == "<!DOCTYPE":
            self._doctype = [] # inside a doctype declaration
        elif self._doctype is not None:
            if prefix == ">":
                self._doctype = None
                return
            text = text.strip()
            if not text:
                return
            self._doctype.append(text)
            n = len(self._doctype)
            if n > 2:
                type = self._doctype[1]
                if type == "PUBLIC" and n == 4:
                    name, type, pubid, system = self._doctype
                elif type == "SYSTEM" and n == 3:
                    name, type, system = self._doctype
                    pubid = None
                else:
                    return
                if pubid:
                    pubid = pubid[1:-1]
                if hasattr(self.target, "doctype"):
                    self.target.doctype(name, pubid, system[1:-1])
                elif self.doctype is not self._XMLParser__doctype:
                    self._XMLParser__doctype(name, pubid, system[1:-1])
                    self.doctype(name, pubid, system[1:-1])
                self._doctype = None


    def doctype(self, name, pubid, system):
        """This method of XMLParser is deprecated."""
        warnings.warn(
            "This method of XMLParser is deprecated.  Define doctype() "
            "method on the TreeBuilder target.",
            DeprecationWarning,
            )

    __doctype = doctype


    def feed(self, data):
        try:
            self._parser.Parse(data, 0)
        except self._error, v:
            self._raiseerror(v)


    def close(self):
        try:
            self._parser.Parse("", 1) # end of data
        except self._error, v:
            self._raiseerror(v)
        tree = self.target.close()
        del self.target, self._parser # get rid of circular references
        return tree

XMLTreeBuilder = XMLParser

try:
    from ElementC14N import _serialize_c14n
    _serialize["c14n"] = _serialize_c14n
except ImportError:
    pass


# PropelForm
class PropelForm:
  defaults = {
    'space':12,
  }
  def __init__(self):
    pass
  @staticmethod
  def spaced_section_box(bool, name):
    box = mforms.newSectionBox(bool, name)
    box.set_padding(PropelForm.defaults['space'])
    box.set_spacing(PropelForm.defaults['space'])
    return box
  @staticmethod
  def spaced_box(bool):
    box = mforms.newBox(bool)
    box.set_padding(PropelForm.defaults['space'])
    box.set_spacing(PropelForm.defaults['space'])
    return box
# PropelObject
class PropelObject(object):
  def __getattr__(self, name):
    if re.search('get_', name):
      if hasattr(self.wbObject, name[4:]):
        return getattr(self.wbObject, name[4:])
      elif self.cache.has_key(name[4:]):
        return self.cache[name[4:]]
      elif self.fields.has_key(name[4:]):
        return self.fields[name[4:]]['default']
      else:
        return "unknown getter"
    else:
        return "unknown attr"
  def __setattr__(self, name, value):
    if re.search('set_', name):
      self.__dict__['cache'][name[4:]] = value
    else:
      self.__dict__[name] = value
# PropelColumn
class PropelColumn(PropelObject):
  fields = {
    'table':{
      'label':'table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'name':{
      'label':'column',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'phpName':{
      'label':'phpName',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'peerName':{
      'label':'peerName',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'primaryKey':{
      'label':'PK',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':False,
      'width':20,
      'optional':True
    },
    'required':{
      'label':'NN',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':False,
      'width':20,
      'optional':True
    },
    'type':{
      'label':'type',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'phpType':{
      'label':'phpType',
      'type':mforms.StringColumnType,
      'items': ['boolean', 'int', 'integer', 'double', 'float', 'string'],
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'sqlType':{
      'label':'sqlType',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'size':{
      'label':'size',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':True
    },
    'scale':{
      'label':'scale',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':True
    },
    'defaultValue':{
      'label':'defaultValue',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':True
    },
    'defaultExpr':{
      'label':'defaultExpr',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'valueSet':{
      'label':'valueSet',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'autoIncrement':{
      'label':'AI',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':False,
      'width':20,
      'optional':True
    },
    'lazyLoad':{
      'label':'lazyLoad',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':True,
      'width':50,
      'optional':True
    },
    'description':{
      'label':'description',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':True
    },
    'primaryString':{
      'label':'primaryString',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':True,
      'width':20,
      'optional':True
    },
    'phpNamingMethod':{
      'label':'phpNamingMethod',
      'type':mforms.StringColumnType,
      'items': ['nochange', 'underscore', 'phpname'],
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'inheritance':{
      'label':'inheritance',
      'type':mforms.StringColumnType,
      'items': ['single', 'false'],
      'default':'false',
      'editable':True,
      'width':100,
      'optional':True
    }
  }
  
  def __init__(self, column, propelTable):
    self.wbObject = column
    self.propelTable = propelTable
    if self.propelTable.wbObject.customData.has_key('columns'):
      self.propelTable.cache['columns'] = pickle.loads(self.propelTable.wbObject.customData['columns'])
    else:
      self.propelTable.cache['columns'] = {}
    if not self.propelTable.cache['columns'].has_key(column.name):
      self.propelTable.cache['columns'][column.name] = {}
    self.cache = self.propelTable.cache['columns'][column.name]
  def wbType2PropelDatatype(self):
    if self.wbObject.userType:
      if self.wbObject.userType.name == 'BOOL':
        return 'BOOLEAN'
      if self.wbObject.userType.name == 'BOOLEAN':
        return 'BOOLEAN'
      return self.wbObject.userType.name
    elif self.wbObject.simpleType:
      if self.wbObject.simpleType.name == 'INT' or self.wbObject.simpleType.name == 'MEDIUMINT':
        return 'INTEGER'
      if self.wbObject.simpleType.name == 'TINYTEXT':
        return 'VARCHAR'
      if self.wbObject.simpleType.name == 'TEXT':
       return 'LONGVARCHAR'
      if self.wbObject.simpleType.name == 'MEDIUMTEXT':
        return 'CLOB'
      if self.wbObject.simpleType.name == 'LONGTEXT':
        return 'CLOB'
      if self.wbObject.simpleType.name == 'DATETIME':
        return 'TIMESTAMP'
      return self.wbObject.simpleType.name
    elif self.wbObject.structuredType:
      return self.wbObject.structuredType.name
  def wbLength2PropelSize(self):
    if self.wbObject.length != -1:
      return str(self.wbObject.length)
    if self.wbObject.simpleType and self.wbObject.simpleType.name == 'TINYTEXT':
      return 16777215
    if self.wbObject.simpleType and self.wbObject.simpleType.name == 'MEDIUMTEXT':
      return 4294967295
    if self.wbObject.simpleType and self.wbObject.simpleType.name == 'DECIMAL':
      return self.wbObject.precision
    else:
      return ''
  def wbLength2PropelScale(self):
    if self.wbObject.scale != -1:
      return str(self.wbObject.scale)
    else:
      return ''
  def __getattr__(self, name):
    if re.search('get_', name):
      if name[4:] == 'table':
        return str(self.wbObject.owner.name)
      elif name[4:] == 'description':
        return str(self.wbObject.comment)
      elif name[4:] == 'type':
        return self.wbType2PropelDatatype()
      elif name[4:] == 'size':
        return self.wbLength2PropelSize()
      elif name[4:] == 'scale':
        return self.wbLength2PropelScale()
      elif name[4:] == 'primaryKey':
        for c in self.wbObject.owner.primaryKey.columns:
          if c.referencedColumn.name == self.wbObject.name:  
            return 1
        return 0
      elif name[4:] == 'required':
        return self.wbObject.isNotNull
    return super(PropelColumn, self).__getattr__(name)
  def save(self):
    for k, v in self.fields.iteritems():
      self.propelTable.cache['columns'][self.wbObject.name] = self.cache
  
# PropelForeignKey
class PropelForeignKey(PropelObject):
  fields = {
    'table':{
      'label':'local table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'name':{
      'label':'name',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'foreignTable':{
      'label':'foreign table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'localColumn':{
      'label':'local column',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'foreignColumn':{
      'label':'foreign column',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'phpName':{
      'label':'phpName',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'refPhpName':{
      'label':'refPhpName',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'onDelete':{
      'label':'onDelete',
      'type':mforms.StringColumnType,
      'default':'NO ACTION',
      'editable':False,
      'width':100,
      'optional':True
    },
    'onUpdate':{
      'label':'onUpdate',
      'type':mforms.StringColumnType,
      'default':'NO ACTION',
      'editable':False,
      'width':100,
      'optional':True
    },
    'skipSql':{
      'label':'skipSql',
      'type':mforms.CheckColumnType,
      'default':0,
      'editable':True,
      'width':100,
      'optional':True
    },
    'defaultJoin':{
      'label':'defaultJoin',
      'type':mforms.StringColumnType,
      'items': ['Criteria::INNER_JOIN', 'Criteria::LEFT_JOIN'],
      'default':'',
      'editable':False,
      'width':100,
      'optional':True
    }
  }
  def __init__(self, foreignKey, propelTable):
    self.wbObject = foreignKey
    self.propelTable = propelTable
    if self.propelTable.wbObject.customData.has_key('foreign_keys'):
      self.propelTable.cache['foreign_keys'] = pickle.loads(self.propelTable.wbObject.customData['foreign_keys'])
    else:
      self.propelTable.cache['foreign_keys'] = {}
    if not self.propelTable.cache['foreign_keys'].has_key(foreignKey.name):
      self.propelTable.cache['foreign_keys'][foreignKey.name] = {}
    self.cache = self.propelTable.cache['foreign_keys'][foreignKey.name]
    
  def __getattr__(self, name):
    if re.search('get_', name):
      if name[4:] == 'table':
        return str(self.wbObject.owner.name)
      elif name[4:] == 'onUpdate':
        return str(self.wbObject.updateRule)
      elif name[4:] == 'onDelete':
        return str(self.wbObject.deleteRule)
      elif name[4:] == 'foreignTable':
        return str(self.wbObject.referencedTable.name)
      elif name[4:15] == 'localColumn':
        return str(self.wbObject.columns[int(name[16:])].name)
      elif name[4:17] == 'foreignColumn':
        return str(self.wbObject.referencedColumns[int(name[18:])].name)
    return super(PropelForeignKey, self).__getattr__(name)
  def save(self):
    for k, v in self.fields.iteritems():
      self.propelTable.cache['foreign_keys'][self.wbObject.name] = self.cache

# PropelIndice
class PropelIndice(PropelObject):
  fields = {
    'table':{
      'label':'table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'name':{
      'label':'name',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'indexType':{
      'label':'type',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'columnName':{
      'label':'colum',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    }
  }
  def __init__(self, indice, propelTable):
    self.wbObject = indice
    self.propelTable = propelTable
    if self.propelTable.wbObject.customData.has_key('indices'):
      self.propelTable.cache['indices'] = pickle.loads(self.propelTable.wbObject.customData['indices'])
    else:
      self.propelTable.cache['indices'] = {}
    if not self.propelTable.cache['indices'].has_key(indice.name):
      self.propelTable.cache['indices'][indice.name] = {}
    self.cache = self.propelTable.cache['indices'][indice.name]
  def __getattr__(self, name):
    if re.search('get_', name):
      if name[4:] == 'table':
        return str(self.wbObject.owner.name)
      elif re.search('columnsName', name):
        tmp = []
        for column in self.wbObject.columns:
          tmp.append(column.referencedColumn.name)
        return tmp
    return super(PropelIndice, self).__getattr__(name)
  def save(self):
    for k, v in self.fields.iteritems():
      self.propelTable.cache['indices'][self.wbObject.name] = self.cache

# PropelBehavior
class PropelBehavior(PropelObject):
  behaviors =  {
    'aggregate_column':('name','foreign_table','expression'),
    'alternative_coding_standards':('brackets_newline','remove_closing_comments','use_whitespace','tab_size','strip_comments'),
    'archivable':('archive_on_insert','archive_on_update','archive_on_delete','archive_class','archive_table','archived_at_column','log_archived_at'),
    'auto_add_pk':('name','autoIncrement', 'type'),
    'delegate': ('to',),
    'i18n':('i18n_columns', 'default_locale', 'locale_column', 'i18n_table', 'i18n_phpname'),
    'nested_set':('left_column', 'right_column', 'level_column', 'use_scope', 'scope_column','method_proxies'),
    'query_cache':('backend', 'lifetime'),
    'sluggable':('slug_column', 'slug_pattern', 'replace_pattern', 'replacement', 'separator', 'permanent'),
    'sortable':('rank_column', 'use_scope', 'scope_column'),
    'timestampable':('create_column', 'update_column'),
    'versionable':('version_table', 'version_column', 'log_created_at', 'log_created_by', 'log_comment')
  }
  
  fields = {
    'table':{
      'label':'table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'name':{
      'label':'behavior',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'items': [
        'aggregate_column',
        'alternative_coding_standards',
        'archivable',
        'auto_add_pk',
        'delegate',
        'i18n',
        'nested_set',
        'query_cache',
        'sluggable',
        'sortable',
        'timestampable',
        'versionable'
    ],
      'optional':False
    },
    'parameter':{
      'label':'param',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'value':{
      'label':'value',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':False
    }
  }
  def __init__(self, behavior, propelTable):
    self.propelTable = propelTable
    if not behavior.has_key('parameters'):
      behavior['parameters'] = {}
    self.cache = behavior
  
  def __getattr__(self, name):
    if name[:14] == 'get_parameter_':
      if self.cache['parameters'].has_key(name[14:]):
        return str(self.cache['parameters'][name[14:]])
      else:
        return ''
    return super(PropelBehavior, self).__getattr__(name)
  def __setattr__(self, name, value):
    if name[:14] == 'set_parameter_':
      self.__dict__['cache']['parameters'][name[14:]] = value
    else:
      return super(PropelBehavior, self).__setattr__(name, value)
  def save(self):
    if not self.propelTable.cache.has_key('behaviors'):
      self.propelTable.cache['behaviors'] = []
    self.propelTable.cache['behaviors'].append(self.cache)

# PropelTable
class PropelTable(PropelObject):
  fields = {
    'name':{
      'label':'table',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':False,
      'width':100,
      'optional':False
    },
    'idMethod':{
      'label':'idMethod',
      'type':mforms.StringColumnType,
      'default':'none',
      'items': ['native', 'none'],
      'editable':True,
      'width':100,
      'optional':True
    },
    'phpName':{
      'label':'phpname',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'package':{
      'label':'package',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'schema':{
      'label':'schema',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'namespace':{
      'label':'namespace',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'skipSql':{
      'label':'skipSql',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'abstract':{
      'label':'abstract',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'isCrossRef':{
      'label':'isCrossRef',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'phpNamingMethod':{
      'label':'phpNamingMethod',
      'type':mforms.StringColumnType,
      'items': ['nochange', 'underscore','phpname', 'clean'],
      'default':'underscore',
      'editable':True,
      'width':100,
      'optional':True
    },
    'baseClass':{
      'label':'baseClass',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'basePeer':{
      'label':'basePeer',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'description':{
      'label':'description',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'heavyIndexing':{
      'label':'heavyIndexing',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'readOnly':{
      'label':'readOnly',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'treeMode':{
      'label':'treeMode',
      'type':mforms.StringColumnType,
      'items': ['NestedSet', 'MaterializedPath'],
      'default':'NestedSet',
      'editable':True,
      'width':100,
      'optional':True
    },
    'reloadOnInsert':{
      'label':'reloadOnInsert',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'reloadOnUpdate':{
      'label':'reloadOnUpdate',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':100,
      'optional':True
    },
    'allowPkInsert':{
      'label':'allowPkInsert',
      'type':mforms.CheckColumnType,
      'default':'0',
      'editable':True,
      'width':20,
      'optional':True
    }
  }
  def __init__(self, table):
    self.wbObject = table
    self.cache = {}
    for k, v in self.fields.iteritems():
      if self.wbObject.customData.has_key(k):
        self.cache[k] = self.wbObject.customData[k]
    self.columns = []
    for column in self.wbObject.columns:
      self.columns.append(PropelColumn(column, self))
    self.foreignKeys = []
    for foreignKey in self.wbObject.foreignKeys:
      self.foreignKeys.append(PropelForeignKey(foreignKey, self))
    self.indices = []
    for indice in self.wbObject.indices:
      self.indices.append(PropelIndice(indice, self))
    self.behaviors = []
    if self.cache.has_key('behaviors'):
      behaviors = self.cache['behaviors']
    elif self.wbObject.customData.has_key('behaviors'):
      behaviors = pickle.loads(self.wbObject.customData['behaviors'])
    else:
      behaviors = []
    for behavior in behaviors:
      self.behaviors.append(PropelBehavior(behavior, self))
  def save(self):
    for k, v in self.fields.iteritems():
      if self.cache.has_key(k):
        self.wbObject.customData[k] = self.cache[k]
    for column in self.columns:
      column.save()
    if self.cache.has_key('columns'):
      self.wbObject.customData['columns'] = pickle.dumps(self.cache['columns'])
    for foreignKey in self.foreignKeys:
      foreignKey.save()
    if self.cache.has_key('foreign_keys'):
      self.wbObject.customData['foreign_keys'] = pickle.dumps(self.cache['foreign_keys'])
    for behavior in self.behaviors:
      behavior.save()
    if self.cache.has_key('behaviors'):
      self.wbObject.customData['behaviors'] = pickle.dumps(self.cache['behaviors'])
    else:
      del self.wbObject.customData['behaviors']
  def erase(self):
    for k in self.cache.keys():
      del self.cache[k]
    for k in self.wbObject.customData.keys():
      del self.wbObject.customData[k]
      
# PropelExternalSchema
class PropelExternalSchema(PropelObject):
  fields = {
    'filename':{
      'label':'filename',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':False
    },
    'referenceOnly':{
      'label':'referenceOnly',
      'type':mforms.CheckColumnType,
      'default':1,
      'editable':True,
      'width':20,
      'optional':False
    }
  }
  def __init__(self, externalSchema, db):
    self.db = db
    self.cache = externalSchema
  
  def save(self):
    if not self.db.cache.has_key('external_schemas'):
      self.db.cache['external_schemas'] = []
    self.db.cache['external_schemas'].append(self.cache)

# PropelDatabase
class PropelDatabase(PropelObject):
  fields = {
    'name':{
      'label':'name',
      'type':mforms.StringColumnType,
      'default':'default',
      'editable':False,
      'width':100,
      'optional':False
    },
    'defaultIdMethod':{
      'label':'default id method',
      'type':mforms.StringColumnType,
      'items': ['native', 'none'],
      'default':'native',
      'editable':True,
      'width':100,
      'optional':False
    },
    'package':{
      'label':'package',
      'type':mforms.StringColumnType,
      'default':'lib.model',
      'editable':True,
      'width':100,
      'optional':True
    },
    'schema':{
      'label':'schema',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'namespace':{
      'label':'namespace',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'baseClass':{
      'label':'baseClass',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'basePeer':{
      'label':'basePeer',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    },
    'defaultPhpNamingMethod':{
      'label':'defaultPhpNamingMethod',
      'type':mforms.StringColumnType,
      'items': ['nochange', 'underscore', 'phpname', 'clean'],
      'default':'underscore',
      'editable':True,
      'width':100,
      'optional':True
    },
    'heavyIndexing':{
      'label':'heavyIndexing',
      'type':mforms.StringColumnType,
      'items': ['True', 'False'],
      'default':'True',
      'editable':True,
      'width':100,
      'optional':True
    },
    'tablePrefix':{
      'label':'tablePrefix',
      'type':mforms.StringColumnType,
      'default':'',
      'editable':True,
      'width':100,
      'optional':True
    }
  }
  def __init__(self, catalog):
    self.wbObject = catalog
    self.tables = []
    self.cache = {}
    for k, v in self.fields.iteritems():
      if self.wbObject.customData.has_key(k):
        self.cache[k] = self.wbObject.customData[k]
    for schema in catalog.schemata:
      for table in schema.tables:
        self.tables.append(PropelTable(table))
    self.externalSchemas = []
    if self.cache.has_key('external_schemas'):
      externalSchemas = self.cache['external_schemas']
    elif self.wbObject.customData.has_key('external_schemas'):
      externalSchemas = pickle.loads(self.wbObject.customData['external_schemas'])
    else:
      externalSchemas = []
    for externalSchema in externalSchemas:
      self.externalSchemas.append(PropelExternalSchema(externalSchema, self))
  
  def __getattr__(self, name):
    if re.search('get_', name):
      if name[4:] == 'name' and self.cache.has_key('name'):
        return self.cache['name']
    return super(PropelDatabase, self).__getattr__(name)
  def save(self):
    for k, v in self.cache.iteritems():
      self.wbObject.customData[k] = v
    for t in self.tables:
      t.save()
    for external in self.externalSchemas:
      external.save()
    if self.cache.has_key('external_schemas'):
      self.wbObject.customData['external_schemas'] = pickle.dumps(self.cache['external_schemas'])
  def erase(self):
    for k in self.cache.keys():
      del self.cache[k]
    for k in self.wbObject.customData.keys():
      del self.wbObject.customData[k]
    for t in self.tables:
      t.erase()
# PropelTab
class PropelTab(mforms.SectionBox):
  widgets = {}
  pass
# PropelTabGrid
class PropelTabGrid(PropelTab):
  defaults = {
    'popup_width':530,
    'popup_height':50,
    'popup_x':110,
    'popup_y':250
  }
  def search(self, grid_name):
    tBox = PropelForm.spaced_box(True)
    self.widgets[grid_name + '_search_label'] = mforms.newLabel("table pattern")
    tBox.add(self.widgets[grid_name + '_search_label'], False, True)
    self.widgets[grid_name + '_search_pattern'] = mforms.newTextEntry()
    tBox.add(self.widgets[grid_name + '_search_pattern'], True, True)
    self.widgets[grid_name + '_search_button'] = mforms.newButton()
    self.widgets[grid_name + '_search_button'].set_text("matching tables")
    self.widgets[grid_name + '_search_match_count'] = mforms.newLabel("")
    self.widgets[grid_name + '_search_button'].add_clicked_callback(lambda: self.find_rows(0))
    tBox.add(self.widgets[grid_name + '_search_button'], False, True)
    tBox.add(self.widgets[grid_name + '_search_match_count'], False, True)
    self.add(tBox, False, True)
  def colmuns_name(self, grid_name):
    for fieldName in self.fields_list:
      self.widgets[grid_name].add_column(self.fields[fieldName]['type'], self.fields[fieldName]['label'], self.fields[fieldName]['width'], self.fields[fieldName]['editable'])
    self.widgets[grid_name].end_columns()
    self.widgets[grid_name].add_activated_callback(getattr(self, 'activate_field'))
    self.widgets[grid_name].set_cell_edited_callback(getattr(self, 'edit_field'))
    self.find_rows(0)
  def select_box(self, grid_name, edited_row, edited_col, fieldName):
    tBox = mforms.Form(None, mforms.FormResizable)
    tBox.set_title("Propel Choice")
    tBox.set_size(self.defaults['popup_width'], self.defaults['popup_height'])
    tBox.set_position(self.defaults['popup_x'], self.defaults['popup_y'])
    box = mforms.newBox(False)
    list = mforms.newSelector(mforms.SelectorPopup)
    list.add_items(self.fields[fieldName]['items'])
    list.set_selected(list.index_of_item_with_title(self.widgets[grid_name].get_string(edited_row, edited_col)))
    box.add(list, False, True)
    ok = mforms.newButton()
    ok.set_text("select this value")
    ok.add_clicked_callback(lambda: self.edit_field(edited_row, edited_col, list.get_string_value()))
    box.add_end(ok, False, True)
    tBox.set_content(box)
    tBox.run_modal(ok, None)
  

# PropelTabDatabase
class PropelTabDatabase(PropelTabGrid):
  fields_list = [
    'name',
    'defaultIdMethod',
    'package',
    'schema',
    'namespace',
    'baseClass',
    'basePeer',
    'defaultPhpNamingMethod',
    'heavyIndexing',
    'tablePrefix'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelDatabase.fields
    super(PropelTabDatabase, self).__init__(bool, name)
    self.widgets['database'] = mforms.newTreeView(1)
    self.colmuns_name()
    self.add_end(self.widgets['database'], True, True)
  def colmuns_name(self):
    self.widgets['database'].add_column(mforms.StringColumnType, 'attribute', 350, False)
    self.widgets['database'].add_column(mforms.StringColumnType, 'value', 350, True)
    self.widgets['database'].end_columns()
    self.widgets['database'].add_activated_callback(getattr(self, 'activate_field'))
    self.widgets['database'].set_cell_edited_callback(getattr(self, 'edit_field'))
    self.find_rows(0)
  def find_rows(self, selected_row):
    self.widgets['database'].clear_rows()
    for fieldName in self.fields_list:
      row = self.widgets['database'].add_row()
      self.widgets['database'].set_string(row, 0, fieldName)
      self.widgets['database'].set_string(row, 1, str(getattr(self.db, 'get_' + fieldName)))
    self.widgets['database'].set_selected(selected_row)
    
  def activate_field(self, edited_row, edited_col):
    fieldName = self.widgets['database'].get_string(edited_row, 0)
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('database', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.widgets['database'].get_string(edited_row, 0)
    setattr(self.db, 'set_' + fieldName, value)
    self.find_rows(self.widgets['database'].get_selected())
    
# PropelTabTables
class PropelTabTables(PropelTabGrid):
  fields_list = [
    'name',
    'idMethod',
    'phpName',
    'package',
    'schema',
    'namespace',
    'skipSql',
    'abstract',
    'isCrossRef',
    'phpNamingMethod',
    'baseClass',
    'basePeer',
    'description',
    'heavyIndexing',
    'readOnly',
    'treeMode',
    'reloadOnInsert',
    'reloadOnUpdate',
    'allowPkInsert'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelTable.fields
    super(PropelTabTables, self).__init__(bool, name)
    self.widgets['tables'] = mforms.newTreeView(1)
    self.search('tables')
    self.colmuns_name('tables')
    self.add_end(self.widgets['tables'], True, True)
  def find_rows(self, selected_row):
    candidates = []
    for table in self.db.tables:
      if self.widgets['tables_search_pattern'].get_string_value() == "" or re.search(self.widgets['tables_search_pattern'].get_string_value(), table.get_name):
        candidates.append(table)
    self.widgets['tables'].clear_rows()
    self.widgets['tables_search_match_count'].set_text("%i table(s) found" % len(candidates))
    for table in candidates:
      row = self.widgets['tables'].add_row()
      for lineNumber, fieldName in enumerate(self.fields_list):
        if self.fields[fieldName]['type'] == mforms.StringColumnType:
          self.widgets['tables'].set_string(row, lineNumber, str(getattr(table, 'get_' + fieldName)))
        elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
          self.widgets['tables'].set_bool(row, lineNumber, int(getattr(table, 'get_' + fieldName)))
    self.widgets['tables'].set_selected(selected_row)    
  def activate_field(self, edited_row, edited_col):
    fieldName = self.fields_list[edited_col]
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('tables', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.fields_list[edited_col]
    tableName = self.widgets['tables'].get_string(edited_row, self.fields_list.index('name'))
    for table in self.db.tables:
      if table.get_name == tableName:
        t = table
        break
    setattr(t, 'set_' + fieldName, value)
    self.find_rows(self.widgets['tables'].get_selected())
    
# PropelTabColumns
class PropelTabColumns(PropelTabGrid):
  fields_list = [
    'table',
    'name',
    'phpName',
    'peerName',
    'primaryKey',
    'required',
    'type',
    'phpType',
    'sqlType',
    'size',
    'scale',
    'defaultValue',
    'defaultExpr',
    'valueSet',
    'autoIncrement',
    'lazyLoad',
    'description',
    'primaryString',
    'phpNamingMethod',
    'inheritance'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelColumn.fields
    super(PropelTabColumns, self).__init__(bool, name)
    self.widgets['columns'] = mforms.newTreeView(1)
    self.search('columns')
    self.colmuns_name('columns')
    self.add_end(self.widgets['columns'], True, True)
  
  def find_rows(self, selected_row):
    candidates = []
    for table in self.db.tables:
      if self.widgets['columns_search_pattern'].get_string_value() == "" or re.search(self.widgets['columns_search_pattern'].get_string_value(), table.get_name):
        candidates.append(table)
    self.widgets['columns'].clear_rows()
    self.widgets['columns_search_match_count'].set_text("%i table(s) found" % len(candidates))
    for table in candidates:
      for column in table.columns:
        row = self.widgets['columns'].add_row()
        for lineNumber, fieldName in enumerate(self.fields_list):
          if self.fields[fieldName]['type'] == mforms.StringColumnType:
            self.widgets['columns'].set_string(row, lineNumber, str(getattr(column, 'get_' + fieldName)))
          elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
            self.widgets['columns'].set_bool(row, lineNumber, int(getattr(column, 'get_' + fieldName)))
    self.widgets['columns'].set_selected(selected_row)
    
  def activate_field(self, edited_row, edited_col):
    fieldName = self.fields_list[edited_col]
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('columns', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.fields_list[edited_col]
    if bool(self.fields[fieldName]['editable']):
      tableName = self.widgets['columns'].get_string(edited_row, self.fields_list.index('table'))
      columnName = self.widgets['columns'].get_string(edited_row, self.fields_list.index('name'))
      for table in self.db.tables:
        if table.get_name == tableName:
          for column in table.columns:
            if column.get_name == columnName:
              c = column
              break
    setattr(c, 'set_' + fieldName, value)    
    self.find_rows(self.widgets['columns'].get_selected())
  
# PropelTabForeignKeys
class PropelTabForeignKeys(PropelTabGrid):
  fields_list = [
    'table',
    'name',
    'foreignTable',
    'localColumn',    
    'foreignColumn',
    'phpName',
    'refPhpName',
    'onDelete',
    'onUpdate',
    'skipSql',
    'defaultJoin'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelForeignKey.fields
    super(PropelTabForeignKeys, self).__init__(bool, name)
    self.widgets['foreign_keys'] = mforms.newTreeView(1)
    self.search('foreign_keys')
    self.colmuns_name('foreign_keys')
    self.add_end(self.widgets['foreign_keys'], True, True)
  
  def find_rows(self, selected_row):
    candidates = []
    
    for table in self.db.tables:
      if self.widgets['foreign_keys_search_pattern'].get_string_value() == "" or re.search(self.widgets['foreign_keys_search_pattern'].get_string_value(), table.get_name):
        candidates.append(table)
    self.widgets['foreign_keys'].clear_rows()
    self.widgets['foreign_keys_search_match_count'].set_text("%i table(s) found" % len(candidates))
    for table in candidates:
      for foreignKey in table.foreignKeys:
        for k, column in enumerate(foreignKey.wbObject.referencedColumns):
          row = self.widgets['foreign_keys'].add_row()
          for lineNumber, fieldName in enumerate(self.fields_list):
            if fieldName == 'localColumn' or fieldName == 'foreignColumn':
              self.widgets['foreign_keys'].set_string(row, lineNumber, str(getattr(foreignKey, 'get_' + fieldName + '_' + str(k))))
            elif self.fields[fieldName]['type'] == mforms.StringColumnType:
              self.widgets['foreign_keys'].set_string(row, lineNumber, str(getattr(foreignKey, 'get_' + fieldName)))
            elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
              self.widgets['foreign_keys'].set_bool(row, lineNumber, int(getattr(foreignKey, 'get_' + fieldName)))
    self.widgets['foreign_keys'].set_selected(selected_row)
  def activate_field(self, edited_row, edited_col):
    fieldName = self.fields_list[edited_col]
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('foreign_keys', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.fields_list[edited_col]
    if bool(self.fields[fieldName]['editable']):
      tableName = self.widgets['foreign_keys'].get_string(edited_row, self.fields_list.index('table'))
      foreignKeyName = self.widgets['foreign_keys'].get_string(edited_row, self.fields_list.index('name'))
      for table in self.db.tables:
        if table.get_name == tableName:
          for foreignKey in table.foreignKeys:
            if foreignKey.get_name == foreignKeyName:
              f = foreignKey
              break
    setattr(f, 'set_' + fieldName, value)
    self.find_rows(self.widgets['foreign_keys'].get_selected())

# PropelTabIndices
class PropelTabIndices(PropelTabGrid):
  fields_list = [
    'table',
    'name',
    'indexType',
    'columnName'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelIndice.fields
    super(PropelTabIndices, self).__init__(bool, name)
    self.widgets['indices'] = mforms.newTreeView(1)
    self.search('indices')
    self.colmuns_name('indices')
    self.add_end(self.widgets['indices'], True, True)
  def find_rows(self, selected_row):
    candidates = []
    for table in self.db.tables:
      if self.widgets['indices_search_pattern'].get_string_value() == "" or re.search(self.widgets['indices_search_pattern'].get_string_value(), table.get_name):
        candidates.append(table)
    self.widgets['indices'].clear_rows()
    self.widgets['indices_search_match_count'].set_text("%i table(s) found" % len(candidates))
    for table in candidates:
      for indice in table.indices:
        for column in indice.get_columnsName:
          row = self.widgets['indices'].add_row()
          for lineNumber, fieldName in enumerate(self.fields_list):
            if fieldName == 'columnName':
              self.widgets['indices'].set_string(row, lineNumber, str(column))
            elif self.fields[fieldName]['type'] == mforms.StringColumnType:
              self.widgets['indices'].set_string(row, lineNumber, str(getattr(indice, 'get_' + fieldName)))
            elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
              self.widgets['indices'].set_bool(row, lineNumber, int(getattr(indice, 'get_' + fieldName)))
    self.widgets['indices'].set_selected(selected_row)
  def activate_field(self, edited_row, edited_col):
    fieldName = self.fields_list[edited_col]
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('indices', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.fields_list[edited_col]
    if bool(self.fields[fieldName]['editable']):
      tableName = self.widgets['indices'].get_string(edited_row, self.fields_list.index('table'))
      indiceName = self.widgets['indices'].get_string(edited_row, self.fields_list.index('name'))
      for table in self.db.tables:
        if table.get_name == tableName:
          for indice in table.indices:
            if indice.get_name == indiceName:
              f = indice
              break
    setattr(f, 'set_' + fieldName, value)
    self.find_rows(self.widgets['indices'].get_selected())

# PropelTabBehaviors
class PropelTabBehaviors(PropelTabGrid):
  fields_list = [
    'table',
    'name',
    'parameter',
    'value'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelBehavior.fields
    self.behaviors = PropelBehavior.behaviors
    super(PropelTabBehaviors, self).__init__(bool, name)
    self.widgets['behaviors'] = mforms.newTreeView(1)
    self.search('behaviors')
    self.colmuns_name('behaviors')
    self.add_remove_behavior_button()
    self.add_end(self.widgets['behaviors'], True, True)
    
  def find_rows(self, selected_row):
    candidates = []
    for table in self.db.tables:
      if self.widgets['behaviors_search_pattern'].get_string_value() == "" or re.search(self.widgets['behaviors_search_pattern'].get_string_value(), table.get_name):
        candidates.append(table)
    self.widgets['behaviors'].clear_rows()
    self.widgets['behaviors_search_match_count'].set_text("%i table(s) found" % len(candidates))
    for table in candidates:
      row = self.widgets['behaviors'].add_row()
      self.widgets['behaviors'].set_string(row, self.fields_list.index('table'), table.get_name)
      for behavior in table.behaviors:
        for parameter in self.behaviors[behavior.get_name]:
          row = self.widgets['behaviors'].add_row()
          for lineNumber, fieldName in enumerate(self.fields_list):
            if fieldName == 'name':
              self.widgets['behaviors'].set_string(row, lineNumber, str(behavior.get_name))
            elif fieldName == 'parameter':
              self.widgets['behaviors'].set_string(row, lineNumber, str(parameter))
            elif fieldName == 'value':
              self.widgets['behaviors'].set_string(row, lineNumber, str(getattr(behavior, 'get_parameter_' + parameter)))
            elif self.fields[fieldName]['type'] == mforms.StringColumnType:
              self.widgets['behaviors'].set_string(row, lineNumber, str(getattr(behavior, 'get_' + fieldName)))
            elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
              self.widgets['behaviors'].set_bool(row, lineNumber, int(getattr(behavior, 'get_' + fieldName)))
    self.widgets['behaviors'].set_selected(selected_row)
    
  def activate_field(self, edited_row, edited_col):
    fieldName = self.fields_list[edited_col]
    if self.fields[fieldName].has_key('items') and len(self.fields[fieldName]['items']) > 0:
      self.select_box('behaviors', edited_row, edited_col, fieldName)
  def edit_field(self, edited_row, edited_col, value):
    if value != '':
      fieldName = self.fields_list[edited_col]
      keys = range(0, self.widgets['behaviors'].get_selected()+1)
      keys.reverse()
      for i in keys:
        if self.widgets['behaviors'].get_string(i, self.fields_list.index('table')):
          tableName = str(self.widgets['behaviors'].get_string(i, self.fields_list.index('table')))
          break
      for table in self.db.tables:
        if table.get_name == tableName:
          t = table
          break
      if fieldName == 'name':
        behaviorName = value
        t.behaviors.append(PropelBehavior({'name':behaviorName}, t))
      elif fieldName == 'value':
        behaviorName = self.widgets['behaviors'].get_string(edited_row, self.fields_list.index('name'))
        parameterName = self.widgets['behaviors'].get_string(edited_row, self.fields_list.index('parameter'))
        for behavior in t.behaviors:
          if behavior.get_name == behaviorName:
            b = behavior
            break
        setattr(b, 'set_parameter_' + parameterName, value)
    self.find_rows(self.widgets['behaviors'].get_selected()+1)
  def add_remove_behavior_button(self):
    tBox = PropelForm.spaced_box(True)
    self.widgets['selectBehaviors'] = mforms.newSelector(mforms.SelectorPopup)
    self.widgets['selectBehaviors'].add_items(PropelBehavior.fields['name']['items'])
    tBox.add(self.widgets['selectBehaviors'], False, True)
    addBehavior = mforms.newButton()
    addBehavior.set_text("add selected behavior to selected table")
    addBehavior.add_clicked_callback(lambda: self.add_behavior())
    tBox.add(addBehavior, False, True)
    removeBehavior = mforms.newButton()
    removeBehavior.set_text("remove this behavior")
    removeBehavior.add_clicked_callback(lambda: self.remove_behavior())
    tBox.add(removeBehavior, False, True)
    self.add_end(tBox, False, True)
  def add_behavior(self):
    behaviorName = str(self.widgets['selectBehaviors'].get_string_value())
    tableName = self.widgets['behaviors'].get_string(self.widgets['behaviors'].get_selected(), self.fields_list.index('table'))
    if behaviorName and tableName:
      for table in self.db.tables:
        if table.get_name == tableName:
          t = table
          break
      t.behaviors.append(PropelBehavior({'name':behaviorName}, t))
      self.find_rows(self.widgets['behaviors'].get_selected()+1)
    else:
      mforms.Utilities.show_warning("Warning", "Please select a table row with a not null 'name' column to add a behavior", "OK", "", "")
  def remove_behavior(self):
    behaviorName = self.widgets['behaviors'].get_string(self.widgets['behaviors'].get_selected(), self.fields_list.index('name'))
    keys = range(0, self.widgets['behaviors'].get_selected()+1)
    keys.reverse()
    tableIndex = 0
    for i in keys:
      if self.widgets['behaviors'].get_string(i, self.fields_list.index('table')):
        tableName = self.widgets['behaviors'].get_string(i, self.fields_list.index('table'))
        break
    for i, table in enumerate(self.db.tables):
        if table.get_name == tableName:
          tableIndex = i
          break
    if behaviorName and tableName:
      for i, behavior in enumerate(self.db.tables[tableIndex].behaviors):
        if behavior.get_name == behaviorName:
          del self.db.tables[tableIndex].behaviors[i]
          break
      self.find_rows(tableIndex)
    else:
      mforms.Utilities.show_warning("Warning", "Please select a behavior row with a not null 'behavior' column to remove this behavior", "OK", "", "")
# PropelTabFile
class PropelTabFile(PropelTab):
  pass
# PropelTabExternalSchemas
class PropelTabExternalSchemas(PropelTabFile):
  fields_list = [
    'filename',
    'referenceOnly'
  ]
  def __init__(self, bool, name, db):
    self.db = db
    self.fields = PropelExternalSchema.fields
    super(PropelTabExternalSchemas, self).__init__(bool, name)
    self.widgets['external_schemas'] = mforms.newTreeView(1)
    self.browse_schema_box()
    self.colmuns_name()
    self.add_end(self.widgets['external_schemas'], True, True)
  def colmuns_name(self):
    for fieldName in self.fields_list:
      self.widgets['external_schemas'].add_column(self.fields[fieldName]['type'], self.fields[fieldName]['label'], self.fields[fieldName]['width'], self.fields[fieldName]['editable'])
    self.widgets['external_schemas'].end_columns()
    self.widgets['external_schemas'].set_cell_edited_callback(getattr(self, 'edit_field'))
    self.find_rows(0)
  def browse_schema_box(self):
    tBox = PropelForm.spaced_box(True)
    label = mforms.newLabel("external propel schema")
    tBox.add(label, False, True)
    self.widgets['external_schema_path'] = mforms.newTextEntry()
    tBox.add(self.widgets['external_schema_path'], True, True)
    self.widgets['external_schema_file'] = mforms.newFileChooser(mforms.OpenFile)
    self.widgets['external_schema_file'].set_extensions('XML files (*.xml)|*.xml','xml')
    self.widgets['external_schema_file'].set_title("external schema file")
    browse = mforms.newButton()
    browse.set_text("Browse")
    browse.add_clicked_callback(lambda: self.browse_schema())
    tBox.add(browse, False, True)
    add = mforms.newButton()
    add.set_text("Add")
    add.add_clicked_callback(lambda: self.add_schema())
    tBox.add(add, False, True)
    self.add(tBox, False, True)
  def browse_schema(self):
    self.widgets['external_schema_file'].run_modal()
    self.widgets['external_schema_path'].set_value(self.widgets['external_schema_file'].get_path())
  def add_schema(self):
    already = False
    for es in self.db.externalSchemas:
      if es['filename'] == self.widgets['external_schema_path'].get_string_value():
          already = True
    if not already:
      if self.widgets['external_schema_path'].get_string_value()!='':
        self.db.externalSchemas.append(PropelExternalSchema({ 'filename':self.widgets['external_schema_path'].get_string_value(), 'referenceOnly':self.fields['referenceOnly']['default'] }, self.db))
    self.find_rows(0)
  def find_rows(self, selected_row):
    self.widgets['external_schemas'].clear_rows()
    for schema in self.db.externalSchemas:
      row = self.widgets['external_schemas'].add_row()
      for lineNumber, fieldName in enumerate(self.fields_list):
        if self.fields[fieldName]['type'] == mforms.StringColumnType:
          self.widgets['external_schemas'].set_string(row, lineNumber, str(getattr(schema, 'get_' + fieldName)))
        elif self.fields[fieldName]['type'] == mforms.CheckColumnType:
          self.widgets['external_schemas'].set_bool(row, lineNumber, int(getattr(schema, 'get_' + fieldName)))
    self.widgets['external_schemas'].set_selected(selected_row)
  def edit_field(self, edited_row, edited_col, value):
    fieldName = self.fields_list[edited_col]
    filename = self.widgets['external_schemas'].get_string(edited_row, self.fields_list.index('filename'))
    for externalSchema in self.db.externalSchemas:
      if externalSchema.get_filename == filename:
        es = externalSchema
        break
    setattr(es, 'set_' + fieldName, value)
    self.find_rows(self.widgets['external_schemas'].get_selected())

# PropelTabExport
class PropelTabExport(PropelTabFile):
  defaults = {
    'export_FK_name':False,
    'export_add_ai_on_pk':False,
    'export_index':False,
    'export_index_name':False,
    'export_unique':False,
    'export_unique_name':False,
    'export_schema_path':'schema.xml',
    'text_editor_width':970,
    'text_editor_height':300
  }
  def __init__(self, bool, name, db):
    self.db = db
    for k, v in self.defaults.iteritems():
      if not self.db.wbObject.customData.has_key(k):
        self.db.cache[k] = v
      else:
        self.db.cache[k] = self.db.wbObject.customData[k]
    super(PropelTabExport, self).__init__(bool, name)
    self.options_schema_box()
    self.text_editor_schema_box()
    self.browse_schema_box()
    self.refresh_text_editor_schema_box()
  def get_xmlized_schema(self):
    database = Element('database')
    for k, v in PropelDatabase.fields.iteritems():
      if self.db.cache.has_key(k) and self.db.cache[k]:
        database.attrib[k] = self.db.cache[k]
      elif not PropelDatabase.fields[k]['optional']:
        database.attrib[k] = PropelDatabase.fields[k]['default']
    database = self.convert_bool_value(database, PropelDatabase)
    for t in self.db.tables:
      table = SubElement(database, 'table')
      for k, v in PropelTable.fields.iteritems():
        if getattr(t, 'get_' + k) and getattr(t, 'get_' + k) != PropelTable.fields[k]['default']:
          table.attrib[k] = getattr(t, 'get_' + k)
        elif not PropelTable.fields[k]['optional']:
          table.attrib[k] = PropelTable.fields[k]['default']
      table = self.convert_bool_value(table, PropelTable)
      for c in t.columns:
        column = SubElement(table, 'column')
        for k, v in PropelColumn.fields.iteritems():
          if k != 'table':
            if getattr(c, 'get_' + k)  and getattr(c, 'get_' + k) != PropelColumn.fields[k]['default']:
              column.attrib[k] = str(getattr(c, 'get_' + k))
            elif c.get_primaryKey and k == 'autoIncrement' and self.widgets['export_add_ai_on_pk'].get_bool_value():
              column.attrib[k] = 'True'
            elif not PropelColumn.fields[k]['optional']:
              column.attrib[k] = PropelColumn.fields[k]['default']
        column = self.convert_bool_value(column, PropelColumn)
      for fk in t.foreignKeys:
        foreign_key = SubElement(table, 'foreign-key')
        for k, v in PropelForeignKey.fields.iteritems():
          if k != 'table' and k != 'localColumn' and k!= 'foreignColumn':
            if k == 'name':
              if self.widgets['export_FK_name'].get_bool_value():
                foreign_key.attrib[k] = str(getattr(fk, 'get_' + k))
            elif getattr(fk, 'get_' + k) and getattr(fk, 'get_' + k) != PropelForeignKey.fields[k]['default']:
              foreign_key.attrib[k] = str(getattr(fk, 'get_' + k))
            elif not PropelForeignKey.fields[k]['optional']:
              foreign_key.attrib[k] = PropelForeignKey.fields[k]['default']
        foreign_key = self.convert_bool_value(foreign_key, PropelForeignKey)
        for k, col in enumerate(fk.wbObject.referencedColumns):
          reference = SubElement(foreign_key, 'reference')
          reference.attrib['local'] = str(getattr(fk, 'get_localColumn_' + str(k)))
          reference.attrib['foreign'] = str(getattr(fk, 'get_foreignColumn_' + str(k)))
      for i in t.indices:
        if (i.get_indexType == 'UNIQUE' and self.widgets['export_unique'].get_bool_value()) or (i.get_indexType == 'INDEX' and self.widgets['export_index'].get_bool_value()):
          type = i.get_indexType.lower()
          indice = SubElement(table, type)
          for k, v in PropelIndice.fields.iteritems():
            if k != 'columnName' and k != 'table' and k != 'indexType':
              if k == 'name':
                if (i.get_indexType == 'UNIQUE' and self.widgets['export_unique_name'].get_bool_value()) or (i.get_indexType == 'INDEX' and self.widgets['export_index_name'].get_bool_value()):
                  indice.attrib[k] = str(getattr(i, 'get_' + k))
              elif getattr(i, 'get_' + k):
                indice.attrib[k] = str(getattr(i, 'get_' + k))
              elif not PropelIndice.fields[k]['optional']:
                indice.attrib[k] = PropelIndice.fields[k]['default']
          for column_name in i.get_columnsName:
            indice_column = SubElement(indice, type + '-column')
            indice_column.attrib['name'] = column_name
          indice = self.convert_bool_value(indice, PropelIndice)
      for k, b in enumerate(t.behaviors):
        behavior = SubElement(table, 'behavior')
        behavior.attrib['name'] = b.get_name
        for p in PropelBehavior.behaviors[b.get_name]:
          if getattr(b, 'get_parameter_' + str(p)) != '':
            parameter = SubElement(behavior, 'parameter')
            parameter.attrib['name'] = p
            parameter.attrib['value'] = getattr(b, 'get_parameter_' + str(p))      
    for k, es in enumerate(self.db.externalSchemas):
      external_schema = SubElement(database, 'external-schema')
      for k, v in PropelExternalSchema.fields.iteritems():
        if getattr(es, 'get_' + k):
          external_schema.attrib[k] = str(getattr(es, 'get_' + k))
        elif not PropelExternalSchema.fields[k]['optional']:
          external_schema.attrib[k] = PropelExternalSchema.fields[k]['default']
      external_schema = self.convert_bool_value(external_schema, PropelExternalSchema)
    self.indent(database)
    return '<?xml version="1.0" encoding="UTF-8"?>\r\n' + tostring(database)
  def convert_bool_value(self, xmltag, associatedClass):
    for k, v in xmltag.attrib.iteritems():
      if associatedClass.fields[k]['type'] == mforms.CheckColumnType:
        if v:
          xmltag.attrib[k] = 'true'
        else:
          xmltag.attrib[k] = 'false'
    return xmltag
  def indent(self, elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            self.indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i
  def text_editor_schema_box(self):
    tBox = PropelForm.spaced_box(True)
    self.widgets['export_text_editor'] = mforms.newTextBox(mforms.BothScrollBars)
    self.widgets['export_text_editor'].set_size(self.defaults['text_editor_width'], self.defaults['text_editor_height'])
    self.widgets['export_text_editor'].set_bordered(True)
    self.widgets['export_text_editor'].set_read_only(False)
    tBox.add(self.widgets['export_text_editor'], False, True)
    self.add(tBox, False, True)
  def options_schema_box(self):
    tBox = PropelForm.spaced_box(True)
    self.widgets['export_FK_name'] = mforms.newCheckBox()
    self.widgets['export_FK_name'].set_text('export all FK name')
    self.widgets['export_FK_name'].set_active(self.db.cache['export_FK_name'])
    self.widgets['export_FK_name'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_FK_name'], False, True)
    self.widgets['export_add_ai_on_pk'] = mforms.newCheckBox()
    self.widgets['export_add_ai_on_pk'].set_text('add AI to PK')
    self.widgets['export_add_ai_on_pk'].set_active(self.db.cache['export_add_ai_on_pk'])
    self.widgets['export_add_ai_on_pk'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_add_ai_on_pk'], False, True)
    self.widgets['export_index'] = mforms.newCheckBox()
    self.widgets['export_index'].set_text('export all indexes')
    self.widgets['export_index'].set_active(self.db.cache['export_index'])
    self.widgets['export_index'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_index'], False, True)
    self.widgets['export_index_name'] = mforms.newCheckBox()
    self.widgets['export_index_name'].set_text('export all indexe\'s name')
    self.widgets['export_index_name'].set_active(self.db.cache['export_index_name'])
    self.widgets['export_index_name'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_index_name'], False, True)
    self.widgets['export_unique'] = mforms.newCheckBox()
    self.widgets['export_unique'].set_text('export all uniques')
    self.widgets['export_unique'].set_active(self.db.cache['export_unique'])
    self.widgets['export_unique'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_unique'], False, True)
    self.widgets['export_unique_name'] = mforms.newCheckBox()
    self.widgets['export_unique_name'].set_text('export all unique\'s name')
    self.widgets['export_unique_name'].set_active(self.db.cache['export_unique_name'])
    self.widgets['export_unique_name'].add_clicked_callback(lambda: self.refresh_text_editor_schema_box())
    tBox.add(self.widgets['export_unique_name'], False, True)
    self.add(tBox, False, True)
  def refresh_text_editor_schema_box(self):
    self.db.cache['export_FK_name'] = self.widgets['export_FK_name'].get_bool_value()
    self.db.cache['export_add_ai_on_pk'] = self.widgets['export_add_ai_on_pk'].get_bool_value()
    self.db.cache['export_index'] = self.widgets['export_index'].get_bool_value()
    self.db.cache['export_index_name'] = self.widgets['export_index_name'].get_bool_value()
    self.db.cache['export_unique'] = self.widgets['export_unique'].get_bool_value()
    self.db.cache['export_unique_name'] = self.widgets['export_unique_name'].get_bool_value()
    self.widgets['export_text_editor'].set_value(self.get_xmlized_schema())
  def browse_schema_box(self):
    tBox = PropelForm.spaced_box(True)
    label = mforms.newLabel("export propel schema")
    tBox.add(label, False, True)
    self.widgets['export_schema_path'] = mforms.newTextEntry()
    self.widgets['export_schema_path'].set_value(self.db.cache['export_schema_path'])
    tBox.add(self.widgets['export_schema_path'], True, True)
    self.widgets['export_schema_file'] = mforms.newFileChooser(mforms.SaveFile)
    self.widgets['export_schema_file'].set_extensions('XML files (*.xml)|*.xml','xml')
    self.widgets['export_schema_file'].set_title("export schema file")
    browse = mforms.newButton()
    browse.set_text("Browse")
    browse.add_clicked_callback(lambda: self.browse_schema())
    tBox.add(browse, False, True)
    save = mforms.newButton()
    save.set_text("Save")
    save.add_clicked_callback(lambda: self.save_schema())
    tBox.add(save, False, True)
    self.add(tBox, False, True)
  def browse_schema(self):
    self.widgets['export_schema_file'].run_modal()
    self.widgets['export_schema_path'].set_value(self.widgets['export_schema_file'].get_path())
    self.db.cache['export_schema_path'] = self.widgets['export_schema_path'].get_string_value()
  def save_schema(self):
    if self.widgets['export_schema_path'].get_string_value() != "":
      f = open(self.widgets['export_schema_path'].get_string_value(),"w")
      f.write(self.get_xmlized_schema())
      f.close()
    else:
      mforms.Utilities.show_warning("Warning", "Please select a xml file to export schema", "OK", "", "")

# ../propel_utility_dev_grt
sys.path.append(str(os.path.dirname(os.path.realpath(__file__))) + '/PropelUtility')
ModuleInfo = DefineModule(name= "Propel Utilities", author= "mazenovi", version="1.0")
class PropelUtilityGUI(mforms.Form):
  
  tabs_list = [
    'database',
    'tables',
    'columns',
    'foreign_keys',
    'indices',
    'behaviors',
    'external_schemas',
    'export'
  ]
  tabs = {
    'database':{
      'name':'Database',
      'class':PropelTabDatabase,
      'method':'PropelTabDatabase'
    },
    'tables':{
      'name':'Tables',
      'class':PropelTabTables,
      'method':'PropelTabTables'
    },
    'columns':{
      'name':'Columns',
      'class':PropelTabColumns,
      'method':'PropelTabColumns'
    },
    'foreign_keys':{
      'name':'Foreign Keys',
      'class':PropelTabForeignKeys,
      'method':'PropelTabForeignKeys'
    },
    'indices':{
      'name':'Indices',
      'class':PropelTabIndices,
      'method':'PropelTabIndices'
    },
    'behaviors':{
      'name':'Behaviors',
      'class':PropelTabBehaviors,
      'method':'PropelTabBehaviors'
    },
    'external_schemas':{
      'name':'External Schemas',
      'class':PropelTabExternalSchemas,
      'method':'PropelTabExternalSchemas'
    },
    'export':{
      'name':'Export',
      'class':PropelTabExport,
      'method':'PropelTabExport'
    }#,
  }
  defaults = {
    'width':1040,
    'height':700,
    'tab_index':0
  }
  widget = {}
  def __init__(self, catalog):
    
    self.db = PropelDatabase(grt.root.wb.doc.physicalModels[0].catalog)
    for k, v in self.defaults.iteritems():
      if not self.db.wbObject.customData.has_key(k):
        self.db.cache[k] = v
      else:
        self.db.cache[k] = self.db.wbObject.customData[k]
    mforms.Form.__init__(self, None, mforms.FormResizable)
    self.set_title("Propel Utility")
    self.set_size(self.defaults['width'], self.defaults['height'])
    box = PropelForm.spaced_box(False)
    box.add(self.header_box(), False, False)
    self.tvMain = mforms.newTabView(False)
    for name in self.tabs_list:
      tTab = self.tabs[name]['class'](False, name, self.db)
      if name == 'export':
        exportTab = tTab
      self.tvMain.add_page(tTab, self.tabs[name]['name'])
    self.tvMain.relayout()
    self.tvMain.set_active_tab(int(self.db.get_tab_index))
    self.tvMain.add_tab_changed_callback(lambda: exportTab.refresh_text_editor_schema_box())
    
    box.add(self.tvMain, True, True)
    self.set_content(box)
    self.ui_ok_cancel_button(box)
 
  def header_box(self):
    headBox = mforms.newBox(True)
    img = mforms.newImageBox()
    
    i = open(str(tempfile.gettempdir() + '/propel-logo.png'), 'wb')
    i.write(binascii.unhexlify('89504e470d0a1a0a0000000d494844520000013a0000005408020000009da96c950000000467414d410000b18f0bfc61050000000970485973000006f4000006f40173eb209a0000001974455874536f667477617265005061696e742e4e45542076332e352e39403cb0cb0000158c49444154785eed5dfb73d5c515e7ffa8af994ea7b555ebf4876aa7b5adb6b6b6d399dacef497fed0fee00b04e44d080408eff71b0221101212decf90040441b012b5151f15022a3eaa33fe54674882610447bffd246bd72ffbddb37b76bf7b6feebdd9cc1d07ef3d7bf69cb3fbd93dfb3a67444ffc8b16881628130b8c281339a398d102d1023d11aeb113440b948d05225ccba6a9a2a0656a81c953aa1e7bfcc9eca7a5a5d555a30857578b45fa6801370b68b1faf8134fb97119a48e70f5305a2c122dc0b540ed9cb95ab8aedf50c76591a28b70f5305a2c122dc0b58016abf8925bfe66ba08573fbbc552d102760bac5cb55a0bd7858b16db0beb28225cfdec164b450bd82d10766a8d6b57bbc52345b4809f05b66e6dd4c2755af50c3f8611aede768b05a3052c1678f2a9515ab8e6315c7486f3582f968d16d05b60dffe035aac8e1b3f218fc9225cf3582f968d16d05b60ccd8715ab81e3adc96c76411ae79ac17cb460be82da0c5eac851a373da2bc235a70163f16801d50253a64ed3c2b5b9b925a7b12a0aaed72ecfec3bf7db2b676ebd72e65b57dff8534ed3c4e2d1027e160878eb5011a0ece17aed724def3f1f003eb31f3f5b334b6d6f695db47849f5f49ad1639e41f33cf1e448dce49ebf606153f37626072dd9baf51bb41f03cf43870e7b5c16170cdbdb3b56ad5e337bf69cf11326e1162b147966dc8499b36a57af599b470b7ed9ad8ddb162c5c5c356dfaa8a7c7a0f6a7463e3db5aa1adfecd8b98bcfc48f5269c151a346cfa899b57af5da96d61d7e0c45a9b973e76be1ba6edd863c6c45d93286ebd537feac45e9d75fbe707b7eeb6439cc993b4f746bf367c2c4c91eb5af59bb5ecb163d40e1b666ed3a6533c3a9ba0d751bc52863fe600cc2d8e1c499437ce4483b8061ad1d0453a65671183ad1305b70d6ec5a27b69238f8d588b418e509d7ae1f9a803a38d3fad99a2ad5bcbd450cff4e9f254b973989410d0469263533676765c0acceac6846cd4c2715408c7181c9dc4a56bfb9813a8d3448e5771b5e11c6af05172e5a62552a4d80c959ab08bf81ccd595135c3f3bff37b12eb57efa5e7dc8c9ca06e2a6a666ce744af5b669d5d39992346e6bd23281732838501b18ccfbe213274d76056a9a9ea9054506c8e5a97dc18245de02e46c4127a471065c6f4550b03ce0dafbcafd56887e4310ce0da64ecf9c7a1e16879c16c2b24dcb562c2c0d3562e167e68fbeee24b096f8e9d163395a6469b0b4c601467e01fcb65583b420d34ba2065c6b03f10d5bea70ed7de95e07a0867383837471d947aded415d82e1f47203f3b623ed795c03a5f6152b5659155108a6cf70f6bd0d2a3bd55ee416846cd480eb2476d93ac3ee400db564c54d310e4ef8342b565a3abaf724808d5caa8157ae5ac3979043e91a00c163b56f16833fc1066fc1b5ebd69b8174e0e021adf08606f2807149ceae8c9d24ed947bed5dffb70ec27638d808381dc9f6b376740e5a141a80017317d5e4d853f5e0692dc2bc438733122b2b0f028e435ea016c492c48caeb1cf8cd76a74f0d0610f5852454a0cae5df7b8babe92beffd2f89c76d9b3779fb90f615773f19265ad3b76a62baaaf6f30af2d054f836c93274f65f65db85bd6611e15515d47d682e362889d1669e7aedd73e72db08a5157b7c96ae44df59bcd7c30d02c5fb172f79ebd6956385bb28a6d3623b8f9b5200e990dde0d4eb3f61f3868d55aab32dacb5ad089a064e0da75b73750fbfef5a093ce5a62f45743270350cd9e58ed9c79e63e6a90d00a124180b3568e9a63c69a0e54b347b8699e58eb9a859937df726204d41938607adcb76fbf410becca789b314f0b4e9c34455b2f8ecd3836afaaaad616c7a634a7389fa604e0da759707507b5fb9afffed497c3dcd94bb76eda17a09fc581cd9712ac2199da1ab511c701c6f85ab3ccbb18a61b8fc80185fd6e282c0200ffaa581c9860d245631cfecdf7f802380f96498e2e0dd82b856a5d517ed6e1e59d292501c38fa3ad10c295ccffec009a83d5df77cd63dd2493d2631d5415d9f5078c0d5ea7e630f83a98561bd5ab7d1eec4ca5a0cbebde1da407b4727a50bbc5ca60ad6f182e2e3d782d432c4b029901560de7cfd2282e90d39596668e0dafbd28f9840ed79f1bbb81de1a4922b312e8b6a1b7bdcf889aeac0cdb545a564b972d37c015ab26be003865a158ed35fa9fd92a806d8a55c396ad944894ee1ea78eaea39e470b5267a4d04259545b9b8092d65ad083a0d870ed7bf5410e50fbce3deca18c4711dc3ad29a9bef7fa62b9d3ebdc6a9f1426115db8f4ef55a0d4571c3755f6d596a42c692de5a579680821ff6a8b2c41e2d4885d5c70e9cabb4e435ef790b5c5971e88b07d7ab6ffec50cd49eb3777ed6ed13da9ca3a796068b93b0ab0eeaa451bbc1b37153bd61c1eca414e6612d2b65fb97c913af619cc0dfd0b0554befba9490e251b5631f4b51c1b505f15e8a6af19d5e6f800a7deb50d1b71870edbf38faca995b08acded2f7daef98dd283819750dc5fb0995532fa75ada7a48abd881ba8e8b19cfcf62b36bf581e7a9c9277897e59bd1a905a94bd71eeeba302c367e03ba669cc62a2c5cafbd577be5853b342f51bbee028639f2158e6673c316adadd1a8de95f2e7ead6d69d54a7741d2cf89d9ba917053fed1d89c58b976a05b01ef950c26096d332c4a6b75284df82d8dba79ac6d5da69198a70ebb088b3ebd93b6f06eaad7dafff81d9630c64c9473549f7c3c9b9db9273b726e7fc1fb5069f13a849497bb1815a9ee1dd8c938970df80408bffda49cb903af10f3e5850fbdb596795d98238302fc41c48ed17b86e833b35774166d7de977f2c81daf3d2bdfd97023c984cceff6c109f99cf659f7de3e0219ba9630cedee6e5bdb9150bd9ceab2cc43ce6c5fc14d06ad6c7b6ebe84240ae2a98a9678d9f2154ebd5012232c869661f6cdad930db34f82f8d78f29457019582b2ae70a949f71502a305cfbcefd6600a82fdc162a5452f2de63c96b77e8813a085d3fcd83876c765a445117d09152c5559d50b017f52e5bae9faba97bea616b8700cc091394d426bc762d03375e8a4a45a8c00a168342f6439deb38f920aecd4ad10783ebd57fff15a7a9016f1a91d3697a823dff530f43040fd94cddb6a5fc22fe12d7ac1dd687a158898a9ce0b76993fe6ef0cc59ac8b7b59d5a8ad20ed137f4a546c536b8d26986f236e0542662d43dca3d072a34e8f1a1b9b3c3a24bf4830b8f2ab3453261f8e4b5efbb6613a4dfde4b970a5ee731f6e3be2a105e510521bbcd4b5588f10278667df1e8a50331bf5a8803a3df2a81a4510cfcd69bea2e00a07c15500c30d138a95d3b8e62a8f81be84e09a5c7c8487d2ff2f5f3faef53384d6d67e8784860baed4cba9802d4db1c2f74e96d9bd9bbc324de55fa2eed3e3aebc53d582d8109846cb0d032ba53bf6f09c04a076b6c11ff790b5aca887efccb0214ee229c42501d7a4fbd76e40853f7ce9513fb5c990cdbc7bfce94a0d776ba9f7e894dbcc0c2fa2a86c802b6edb33ed6388a264e8fad48e14b3d23419c5cae0bb52f7f28541f832185eb11b1e2a061c70f9a20aca21866b72e12167a002ab171f71d553d20759ec018d06a81892ed866d69830cf80973a6d94a783c60808ad9ddd056cd793e9e160951cb0c2a1862a6521bfb821be728650b91cd5170303c3344b860adcc8887eadd27f905870caec985077d800aac5ef8255f3d85920cd97c73345dc34d576b4415bc5fa7c483b3a46de9e0216d652dd4becbc18387cc7186cd2e25e5bb2ade23820953a6303f3684fc66ef808ab42215d7de2e16c250bbdfb2aca105519cbf77eddd4b0d058700aec09b275081d5b77e92c70accc94d90e16c0640c23d55b41f27d0018a98cf1b99b5f31564c6a901f6907000d302f63339450c7d5dc8a655443961c663004186da31f3e0950f62f31b26f3344fce733fb367217fc5d62e6669b40b02b5738a98b30750b7a3f2dc84e33777b19de1e4c22ffc81dafdabe46372b4e6e84c856c5662d8222a0fa75db334e696a6bcaf3c2d6df6c9fdb4b03ef5a4ce3c10f325dd0a9c80385909319a20a205a73539e38e9305387b8dd4563c47e02034459a5d93f33ff701ea5bf727ff71bb9467300ad38d716a6341cc59b305bf986198e83c54104570946aed554c1fc14306a7370986d3178faa39d93da8ede880590eacc62f385c93f30fb801f5cd7b93f70307a48215a8e7c8ca113c7505d7d0030c1b4bd2fab8c1a7e5903faa65a840be9ced99c185df0aad2278679fee6a8677bf94253d926879c092233c8519842bd016e7c788b1a2d14a5040b826177fcf05eaebdf4b2effdd2a6b1e02e6e309a71ec03f63a4f675fc2e662876602e0829d5068251b1f38e16626aa58e76adcd6ddd70b2b6a6351c695a062d37384d563903121404aec01e0ba8dd450a1941b52b8eddd2a644c2256b030b02cc45fc104ad4de8ceb81bea1d5fd027003a84eb79429fb28e102e16e30cde8e4fd6ad5373cf137cb80bd372714e1adaf96218e949cf8e4240e0c572c35f1a8cd84d537be9fbc5fd490113010b5af8bf42d69f351b7ead2ede471e84205c554e215e76c48ebd1485a0bccf61e095499b70e3958c52e714e7dd3c5cdb15a1579b0e3e851b547de5d8f5aac4542c23579fd3b04506fcb73b1c1aa839540db81949dc08e8e4e6a2f0a8e34d666080f6fad484b8088a1ba4f414ed5710442bda48511b0a4447c7d3f2df030456b46e5812ef5b8546cc815f49a9e21d4335a10a771cc14047ef6294ea930704db03f947d89fae6ddc9076e17380ba13315b2d9b0600374c3e64a28845e1c9e787bc421e3d050cb6f43591cc904599c73c45368502fae8278142cf12279e19ad9f8bd3db914206484b0daa953adeded1bf139768c8c9769b5af764e708d8764ada5e209b466e49c6055bc658aa9a03f5c934b7ffc6646c5ecfa41809011d0fcb9e7b60b882a1f3fa350219b399966fc6aacc85293882c3e7ed1032bd244c551ca07ae03111ee0fa22c8c3db8f0694528b52f9a55f45cc83073fe6c3a794d68c4e51cb878fad0aaaa91b5c938faa93b7ee4b3e740e6f6fd0e1f8f16d66a0e2573f1350219bbd83f4f98951eea5a85b9938442977d5ca4e7e37b88655cf8a524170e244935fbdcc5b877ecc874fa9e8a1944e5b0f015ca9d529855e3f6351f1dacd39d4fceaaae052d4d55cf32bb30a36c8d0aa5654b81e3fdec49c51732e596153e6adc3a1b57ee9d71ea7d6926aa322c1d503a8de4b56d8774842369754bb0611864a7e83085541f84726ae1628385c393b49014f6e84fe54c866a78bbeaea6ac3c7ae6adc3ca53bc64352a205cbd819a675e1586d6ba7054da88926d9ba1156ccfde7d5a33e6bf973fb47a9575ed0581ab37503b3a1c927c53761faa90cd65dd0fb2c253f7e699d11e2acc1a25a24e60b83efb6ca3eb6652feb9543165dc1d09d2b7b4660cf8e82f8890c38d4930b8ba02f5d4a91dc3cdd651df68819c16080057fe39ea8913cd39c58dc5a30586b305f2c2d5eafa7674c4ab6ac3b98345dd435ac01fae66a09e3cd91252ccc82b5a205ac02fe90605d4cece3891c63e152d50400bb8cdae9d9d9bb358c523f2020a58c2ac11970ca19ba83c65252cb883688614b5549e6207ee4149115986197e3568b54565c685abb2f10bdc1655ccd2ab0ccf7df08a00e97d030619d9dcb065e9d29b02f60eb9de38ce092203f26b29011383b0e533c131329fb86429ed70951158821f9096ac51ac8221eca05f8e70648ec93297018d3039f05fba68af2bb477745a8537132812e6816b3a5013c2e41b128539c9acb56196433a141e1282e451c449bc82125be00a881e3dda505009ca9139f25c29cd8fffc50d5be4f6c5ac8b50d178a62712c90aed449845f457f1d45b7c89444ff812d7e545a63611c517097bace102816af047346dc94a045246a41bcc6032c8036e778139c2fc0b86c8ce28b3ce22c6524b262422062044874482b6749808b0c5f7005bfa4b91e95c6881f82fd002b18511eb183162c497d01df4485a8b1090f85f91ee19fa227f9fd2dc088d8fe288632aae79cb5f716314b6c28547592fc8f08116883a0032d4885a66d70e449914a5f0abcc910961e0a7c86ba7a8051f593b7ca24199978040a471a8abdb848a50dde429556931f025e2cecafc912dad3bbe5676d4684e9e8ee07ddb02d7e0f5550c43f412b4ab1ce9d3dd57fa5d92006ddcd979f4abafbe6a6a6a9e5a552d8c80bea2649ae0cc0048a986ce7de3c68d77de79577665147ceee449f0afaf6f90d3bec20d214bc5d20ed372366e0b0a026cfffdf4d3244990ac518c2048c385fcd1608b3ff44e9941070f9e10185168815a4e9e3cf5f9e79f239fba802b1273544d1bc040fa8f520d46686b3b824a7b7a7a2195981271770a8f81bef8e28bb367bbd23a2a0c955d0359058497181645204f3aac1428fff1e259c88ce58c802b0632e9d7a0b1f60f4690c4b0ab446346c1e74f9f4641c4888e702d332c8b4ca1b2d78a7fac59bb0e736cfa4b444845a7bc7efd3a3a253a87cc329c8513e2005b4d8052e84f608899b061cb4080484ccb00cf975f7e09fe488b2873ba8a08f7e9b95ad488dc05c0a152111802cfe0803fcc42221106dc01f9259402744529e49e1451f031db8027ba354492532bbe5102a6c22b366c59755fbc884afbfbfbc147046a17ae0a3ec81605308b4a9521464e745291b43de13ea453a2a47f12a99ca12f649690c3bc2a5f6b65db54542142288b821834ad2d5508821185603a7c78a2fdc4842073a5a0d7caf0fca2e1316cc34d458fc4dc856fe48a4e812b7c3f013ff35f769a82ab06c70ffc3ff9e493ecafe96ff06ff890daa0ad0393e4a9e7c1e4fa8d1be8eb8d8d4d10035df3f4e933f8b2b77760ea93ae265cf1d6d69d20c07fe13d2a02676540a230e9876789fbfafa50c5cb2fbf42e1444045bb5305b3235d98e0a9a4ab110b01f1d34d706ddc961d16152ba10862882b0c91dc557a46b6562ad4ef11ae3e9685bb05e70a38115d1f3b19721f45363cda5b84e14586327c090c8b6c71c27f462753bc297447e56d9a36d71392b588590e8e9c105d4c71f00c450074f1251678f81e672d69708ad4efdac4e458c8410078d4f01ba5186272c6f882493b9d8736cd1334f0f051a3043322d70962b1c8c41f722048e7396d6e5806a3c0b163cfc22380b3209706f8b748de2373c682279c085916356268c34029f585332f1d6091d103e3a69ce441b6b5719b2c3ed01c83c34d9a40fe9a76bfe10a61f70e1170c4af2808d08ae6f3e937b9cb8cc8cd613832803788ce0d3c08e5918d526ec9ca93187871a269f187cea16cf9a2a364130d62124e9365a729c10d0e2ab66dd2c9945091a857ba8e90071369365f2bc51365a10ed80aec893f0c49625452e2092b4cd09b95f3278c4d18bfd2ce0284c188a0f4154cb9181d802e78da4a4e13001510c59b5b51249bd80635a2152443ec1ec926c02a40a90bc3019c97f442140bec74aa7b39f9635350ae26c01ca3955cda88bae02928d93115a50afabf230aca3d32cf6381e0afd5b05f9a5db5ba4a88fd5eb9fc762dabd0a7578c39590d93e211ae25dad069df2fbf88f08ab16d032f200f2b9cfd605e95a71a795889b286a93e3ff38ae410e15a91cd1a95aa4c0b44b85666bb46ad2ad20211ae15d9ac51a9cab440846b65b66bd4aa222d10e15a91cd1a95aa4c0bfc0fcec7ad94945e312e0000000049454e44ae426082'))
    i.close()
    img.set_image(str(tempfile.gettempdir() + '/propel-logo.png'))
  
    img.set_size(self.defaults['width']/2-PropelForm.defaults['space']*2, 100)
    headBox.add(img, False, False)
    return headBox
  def ui_ok_cancel_button(self, box):
    tBox = PropelForm.spaced_box(True)
    self.cancelButton = mforms.newButton()
    self.cancelButton.set_text("cancel")
    tBox.add_end(self.cancelButton, False, True)
    self.okButton = mforms.newButton()
    self.okButton.set_text("ok")
    tBox.add_end(self.okButton, False, True)
    self.okButton.add_clicked_callback(self.save)
    box.add_end(tBox, False, True)
  def save(self):
    self.db.set_tab_index = self.tvMain.get_active_tab()
    self.db.save()
  def run(self):
    self.run_modal(self.okButton, self.cancelButton)
@ModuleInfo.plugin("wb.catalog.util.PropelUtility", caption= "Propel Utility", input= [wbinputs.currentCatalog()], pluginMenu= "Catalog", type="standalone")
@ModuleInfo.export(grt.INT, grt.classes.db_Catalog)
def PropelUtility(catalog):
  form = PropelUtilityGUI(catalog)
  form.run()
  return 0
@ModuleInfo.plugin("wb.catalog.util.PropelErase", caption= "Propel Erase All Data", input= [wbinputs.currentCatalog()], pluginMenu= "Catalog", type="standalone")
@ModuleInfo.export(grt.INT, grt.classes.db_Catalog)
def PropelErase(catalog):
  if mforms.Utilities.show_warning("Warning", "All propel data of you will be lost", "OK", "NO IT'S A JOKE", ""):
    db = PropelDatabase.PropelDatabase(grt.root.wb.doc.physicalModels[0].catalog)
    for k, v in PropelUtilityGUI.defaults.iteritems():
      if not db.wbObject.customData.has_key(k):
        db.cache[k] = v
      else:
        db.cache[k] = db.wbObject.customData[k]
    db.erase()
  return 0