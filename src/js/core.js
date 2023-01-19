const debounce = (func, delay) => {
    let debounceTimer;
    return function() {
        const context = this;
        const args = arguments;
        clearTimeout(debounceTimer);
        debounceTimer = setTimeout(() => func.apply(context, args), delay);
    };
};
var _id = (id) => {
    return document.getElementById(id);
};

var merge = (a,b) => {
    return Object.assign({}, a,b);
};

var is_alphanum=(c)=>{
    return  (c || false) && c.match(/[-.a-zA-Z0-9]/i) !== null;
};

var is_in_key = (txt, sym) =>{
    var line_start  = txt.lastIndexOf("\n");
    var quote_count = 0;
    for(var i = txt.length - 1 ; i > line_start; i--){
        if(txt[i] == '"'){ quote_count = quote_count + 1;}
    }
    var lastc = txt[line_start+1];
    return (lastc == ':' || lastc == '^')  && !(quote_count & 1);
};


var set = (nel, attrs) => {
    if(!nel) { return nel; }
    if(attrs.class) { attrs.class.forEach((c)=>{ nel.classList.add(c); }); }
    if(attrs.text) { nel.innerText = attrs.text; }
    if(attrs.html) { nel.innerHTML = attrs.html; }
    if(attrs.src) { nel.src = attrs.src; }
    if(attrs.autofocus) { nel.autofocus = "true"; }
    if(attrs.intoView) {
        nel.scrollIntoView({
            behavior: "smooth", 
            block: "end",
            inline: "nearest"
        });
    }
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
            if(def){
                def.append = nel;
                nel.els[id] = el(def);
            }
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

var main = (f)=>{
    document.addEventListener('DOMContentLoaded', ()=> {
        f();
    }, false);
};

// end of lib

var update_widgets = ()=> {
    for(var el of document.getElementsByClassName("widget")){
        var url = el.dataset.url;
        if(url){
            el.innerHTML = "<i class='fa fa-spinner fa-spin'></i>";
            fetch(url).then((resp)=>{
                window.resp = resp;
                if(resp.status < 300){
                    resp.text().then((t)=>{
                        el.innerHTML = t;
                    });
                } else {
                    console.log('ups',resp);
                    el.innerHTML = `Error ${resp.status}`;
                }
            });
        }
    }
}

main(()=>{
    update_widgets();
})
