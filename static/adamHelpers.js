window.adamHelpers = {
  setInnerHtml : function (el, html) {
    el.innerHTML = html;
  },
  addOnload : function (f) {
    window.addEventListener("load", f);
  },
  getWindow : function () {
    return (function () { return this; })();
  },
  getDocument : function () {
    return window.document;
  },
  attr : function (o, p) {
    return o[p];
  }
// Not used
//  addEvent : function (el, event, f) {
//    el.addEventListener(event, f);
//  },
//  stopProp : function (e) {
//    e.stopPropagation();
//  },
//  preventDefault : function (e) {
//    e.preventDefault();
//  },
//  appendChild : function (parent, el) {
//    parent.appendChild(el);
//  },
};
