var _id = (id) => {
    return document.getElementById(id);
};

var merge = (a,b) => {
    return Object.assign({}, a,b);
};

var transparent = 'transparent';
var absolute = 'absolute';
var none = 'none';
var relative = 'relative';


var set = (nel, attrs) => {
    if(!nel) { return nel; }
    if(attrs.class) { attrs.class.forEach((c)=>{ nel.classList.add(c); }); }
    if(attrs.text) { nel.innerText = attrs.text; }
    if(attrs.on){
        for(e in attrs.on){
            var f = attrs.on[e];
            nel.addEventListener(e,f);
        }
    }
    if(attrs.style) {
        for(s in attrs.style){
            var v = attrs.style[s];
            if(Number.isInteger(v)){
                v = v + 'px';
            }
            nel.style[s] = v;
        }
    }
    if(attrs.value){
        nel.value = attrs.value;
    }
    if(attrs.spellcheck != undefined){
        nel.spellcheck = attrs.spellcheck.toString();
    }
    if(attrs.els){
        nel.els = {};
        nel.innerHTML = '';
        for(id in attrs.els){
            var def = attrs.els[id];
            def.append = nel;
            nel.els[id] = el(def);
        }
    }
    if(attrs.append){
        attrs.append.appendChild(nel);
    }
    return nel;
};

var el = (attrs) => {
    var nel= null;
    if(attrs.tag === 'text'){
        nel= document.createTextNode("");
    } else {
        nel = document.createElement(attrs.tag);
    }
    set(nel, attrs);
    return nel;
};

var main = (f) => {
    document.addEventListener('DOMContentLoaded', ()=> {
        f();
    }, false);
}
