// Generated by CoffeeScript 1.6.1
(function() {
  var XMLNode, _,
    __hasProp = {}.hasOwnProperty;

  _ = require('underscore');

  module.exports = XMLNode = (function() {

    function XMLNode(parent) {
      this.parent = parent;
      this.stringify = this.parent.stringify;
    }

    XMLNode.prototype.element = function(name, attributes, text) {
      var attKey, attVal, item, key, lastChild, piKey, piVal, val, _i, _len, _ref;
      lastChild = null;
      if (attributes == null) {
        attributes = {};
      }
      if (!_.isObject(attributes)) {
        _ref = [attributes, text], text = _ref[0], attributes = _ref[1];
      }
      if (_.isArray(name)) {
        for (_i = 0, _len = name.length; _i < _len; _i++) {
          item = name[_i];
          lastChild = this.element(item);
        }
      } else if (_.isFunction(name)) {
        name = name.apply();
        lastChild = this.element(name);
      } else if (_.isObject(name)) {
        for (key in name) {
          if (!__hasProp.call(name, key)) continue;
          val = name[key];
          if (_.isFunction(val)) {
            name[key] = val.apply();
          }
        }
        for (key in name) {
          if (!__hasProp.call(name, key)) continue;
          val = name[key];
          if (val == null) {
            delete name[key];
          }
        }
        for (attKey in name) {
          if (!__hasProp.call(name, attKey)) continue;
          attVal = name[attKey];
          if (this.stringify.convertAttKey && attKey.indexOf(this.stringify.convertAttKey) === 0) {
            this.attribute(attKey.substr(this.stringify.convertAttKey.length), attVal);
            delete name[attKey];
          }
        }
        for (piKey in name) {
          if (!__hasProp.call(name, piKey)) continue;
          piVal = name[piKey];
          if (this.stringify.convertPIKey && piKey.indexOf(this.stringify.convertPIKey) === 0) {
            this.instruction(piKey.substr(this.stringify.convertPIKey.length), piVal);
            delete name[piKey];
          }
        }
        for (key in name) {
          if (!__hasProp.call(name, key)) continue;
          val = name[key];
          if (_.isObject(val)) {
            if (this.stringify.convertListKey && key.indexOf(this.stringify.convertListKey) === 0 && _.isArray(val)) {
              lastChild = this.element(val);
            } else {
              lastChild = this.element(key);
              lastChild.element(val);
            }
          } else {
            lastChild = this.element(key, val);
          }
        }
      } else {
        name = '' + name;
        if (this.stringify.convertTextKey && name.indexOf(this.stringify.convertTextKey) === 0) {
          lastChild = this.text(text);
        } else if (this.stringify.convertCDataKey && name.indexOf(this.stringify.convertCDataKey) === 0) {
          lastChild = this.cdata(text);
        } else if (this.stringify.convertCommentKey && name.indexOf(this.stringify.convertCommentKey) === 0) {
          lastChild = this.comment(text);
        } else if (this.stringify.convertRawKey && name.indexOf(this.stringify.convertRawKey) === 0) {
          lastChild = this.raw(text);
        } else {
          lastChild = this.node(name, attributes, text);
        }
      }
      return lastChild;
    };

    XMLNode.prototype.insertBefore = function(name, attributes, text) {
      var child, i, removed;
      if (this.isRoot) {
        throw new Error("Cannot insert elements at root level");
      }
      i = this.parent.children.indexOf(this);
      removed = this.parent.children.splice(i);
      child = this.parent.element(name, attributes, text);
      Array.prototype.push.apply(this.parent.children, removed);
      return child;
    };

    XMLNode.prototype.insertAfter = function(name, attributes, text) {
      var child, i, removed;
      if (this.isRoot) {
        throw new Error("Cannot insert elements at root level");
      }
      i = this.parent.children.indexOf(this);
      removed = this.parent.children.splice(i + 1);
      child = this.parent.element(name, attributes, text);
      Array.prototype.push.apply(this.parent.children, removed);
      return child;
    };

    XMLNode.prototype.remove = function() {
      var i, _ref;
      if (this.isRoot) {
        throw new Error("Cannot remove the root element");
      }
      i = this.parent.children.indexOf(this);
      [].splice.apply(this.parent.children, [i, i - i + 1].concat(_ref = [])), _ref;
      return this.parent;
    };

    XMLNode.prototype.node = function(name, attributes, text) {
      var XMLElement, child;
      XMLElement = require('./XMLElement');
      child = new XMLElement(this, name, attributes);
      if (text != null) {
        child.text(text);
      }
      this.children.push(child);
      return child;
    };

    XMLNode.prototype.text = function(value) {
      var XMLText, child;
      XMLText = require('./XMLText');
      child = new XMLText(this, value);
      this.children.push(child);
      return this;
    };

    XMLNode.prototype.cdata = function(value) {
      var XMLCData, child;
      XMLCData = require('./XMLCData');
      child = new XMLCData(this, value);
      this.children.push(child);
      return this;
    };

    XMLNode.prototype.comment = function(value) {
      var XMLComment, child;
      XMLComment = require('./XMLComment');
      child = new XMLComment(this, value);
      this.children.push(child);
      return this;
    };

    XMLNode.prototype.raw = function(value) {
      var XMLRaw, child;
      XMLRaw = require('./XMLRaw');
      child = new XMLRaw(this, value);
      this.children.push(child);
      return this;
    };

    XMLNode.prototype.up = function() {
      if (this.isRoot) {
        throw new Error("The root node has no parent. Use doc() if you need to get the document object.");
      }
      return this.parent;
    };

    XMLNode.prototype.root = function() {
      var child;
      if (this.isRoot) {
        return this;
      }
      child = this.parent;
      while (!child.isRoot) {
        child = child.parent;
      }
      return child;
    };

    XMLNode.prototype.document = function() {
      return this.root().documentObject;
    };

    XMLNode.prototype.end = function(options) {
      return this.document().toString(options);
    };

    XMLNode.prototype.prev = function() {
      var i;
      if (this.isRoot) {
        throw new Error("Root node has no siblings");
      }
      i = this.parent.children.indexOf(this);
      if (i < 1) {
        throw new Error("Already at the first node");
      }
      return this.parent.children[i - 1];
    };

    XMLNode.prototype.next = function() {
      var i;
      if (this.isRoot) {
        throw new Error("Root node has no siblings");
      }
      i = this.parent.children.indexOf(this);
      if (i === -1 || i === this.parent.children.length - 1) {
        throw new Error("Already at the last node");
      }
      return this.parent.children[i + 1];
    };

    XMLNode.prototype.importXMLBuilder = function(xmlbuilder) {
      var clonedRoot;
      clonedRoot = xmlbuilder.root().clone(true);
      clonedRoot.parent = this;
      this.children.push(clonedRoot);
      clonedRoot.isRoot = false;
      return this;
    };

    XMLNode.prototype.clone = function(deep) {
      return _.clone(this);
    };

    XMLNode.prototype.ele = function(name, attributes, text) {
      return this.element(name, attributes, text);
    };

    XMLNode.prototype.nod = function(name, attributes, text) {
      return this.node(name, attributes, text);
    };

    XMLNode.prototype.txt = function(value) {
      return this.text(value);
    };

    XMLNode.prototype.dat = function(value) {
      return this.cdata(value);
    };

    XMLNode.prototype.com = function(value) {
      return this.comment(value);
    };

    XMLNode.prototype.doc = function() {
      return this.document();
    };

    XMLNode.prototype.e = function(name, attributes, text) {
      return this.element(name, attributes, text);
    };

    XMLNode.prototype.n = function(name, attributes, text) {
      return this.node(name, attributes, text);
    };

    XMLNode.prototype.t = function(value) {
      return this.text(value);
    };

    XMLNode.prototype.d = function(value) {
      return this.cdata(value);
    };

    XMLNode.prototype.c = function(value) {
      return this.comment(value);
    };

    XMLNode.prototype.r = function(value) {
      return this.raw(value);
    };

    XMLNode.prototype.u = function() {
      return this.up();
    };

    return XMLNode;

  })();

}).call(this);
