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

var transparent = 'transparent';
var black = '#1f2937';
var absolute = 'absolute';
var none = 'none';
var relative = 'relative';


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



var insert = (textarea, symbol) => {
    var s = textarea.value;
    var pos = textarea.selectionEnd;
    textarea.value = s.substring(0,pos) + symbol + s.substring(pos, s.length);
    textarea.selectionEnd = pos;
}
var auto_close = (key, textarea) => {
    switch (key) {
    case '{': insert(textarea, '}'); break;
    case '[': insert(textarea, ']'); break;
    case '(': insert(textarea, ')'); break;
    case '"': insert(textarea, '"'); break;
    }
}

var hl = (ctx, v)=>{
    var t = ctx.editor.els.textarea;
    var h = ctx.editor.els.hl;
    var c = ctx.editor.els.cursor;
    set(t, {style: {opacity: "1"}});
    set(h, {style: {opacity: "0"}});
    h.innerHTML =
        v
        .replace(/:[a-zA-Z0-9][-.:\/_a-zA-Z0-9]+/gi, (x)=> {return `<b style='color:gold'>${x}</b>`;})
        .replace(/#[-.:_a-zA-Z0-9]+/gi, (x)=> {return `<b style='color:blue'>${x}</b>`;})
        .replace(/\^[-_a-zA-Z0-9]+/gi, (x)=> {return `<b style='color:purple'>${x}</b>`;})
        .replace(/("[^"]+")/gi, (x)=> { return `<b style='color:green'>${x}</b>`; })
        .replace(/(\(\([^\)]+\)\))/gi, (x)=> { return `<b style='color:purple'>${x}</b>`; })
        .replace(/(\[\[[^\]]+\]\])/gi, (x)=> { return `<b style='color:purple'>${x}</b>`; })
    ;
    set(h, {style: {opacity: "1"}});
    set(t, {style: {opacity: 0.5}});

    setTimeout(()=> {
        var rect = h.getBoundingClientRect();
        set(t, {style: {width: rect.width, height: rect.height + 500}});
        set(c, {style: {width: rect.width, height: rect.height + 500}});
    }, 300);

};

var keys = [':title', ':tags', ':desc', ':role'];
var options = {
    key: (ctx, token)=> {
        if( token.startsWith(':fa-')){
            return ctx.icons.search(token.substring(4)).map((x)=> { return `:fa-${x.item}`; });
        } else {
            return ctx.keys.search(token).map((x)=> { return x.item; });
        }
    },
    symbol: (ctx, token)=>{
        return ctx.symbols.search(token).map((x)=> { return x.item; });
    }
};

var hide_popup = (ctx)=>{
    var els = ctx.editor.els;
    ctx.items = null;
    set(els.pop, {style: {display: 'none'}});
};

var selection_style = {background: 'blueviolet'};

var insert_item = (ctx,item) => {
    var textarea = ctx.editor.els.textarea;
    var v = textarea.value;
    var start = textarea.selectionStart;
    var new_v = v.substring(0, ctx.insert_at) + item.name + v.substring(start, v.length);
    textarea.value = new_v;
    textarea.selectionEnd = ctx.insert_at + item.length;
    hl(ctx, new_v);
    hide_popup(ctx);
};

var insert_selection = (ctx) => {
    var item = ctx.items[ctx.selection];
    insert_item(ctx,item);
};

var autocompl = (ctx, v)=> {
    var els = ctx.editor.els;
    var btxt = v.substring(0,els.textarea.selectionStart);

    var line_start  = btxt.lastIndexOf("\n");
    var str_start   = btxt.lastIndexOf('"');
    var colon_start = btxt.lastIndexOf(':');
    var ann_start = btxt.lastIndexOf('^');
    var space_start = btxt.lastIndexOf(' ');
    var token_start = -1;

    // for(var i = btxt.length; i > 0; i--){
    // TODO replace with backward search by symbols
    // TODO add annotations support
    //     console.log(i, btxt[i]);
    //     // if " -> in string
    //     // if : -> in key -> check line beginning
    //     // if ^
    // }

    var type = null;
    if(str_start > line_start && str_start > colon_start) {
        token_start = str_start;
        type = 'string';
    } else if (colon_start > line_start && space_start > colon_start ) {
        token_start = space_start + 1;
        type = 'symbol';
    } else if (colon_start > line_start && space_start < colon_start) {
        token_start = colon_start;
        type = 'key';
    }
    if(type == 'symbol' || type == 'key') {
        var token = btxt.substring(token_start);
        var items = options[type](ctx, token);
        if(items.length > 0) {
            ctx.items = items;
            ctx.selection = 0;
            ctx.insert_at = token_start;
            var atxt = v.substring(0, token_start);
            els.caret.els.txt.textContent = atxt;
            els.caret.els.cursor.innerText = token;
            var cur = els.caret.els.cursor;
            var item_els = {};
            for (let i = 0; i < items.length; i++) {
                var item =items[i];
                var icon = null;
                if(item.logo) {
                    icon = {tag: 'img', src: item.logo, style: { height: 16, 'border-radius': "1px", 'padding-right': '0.5rem', display: 'inline-block'}};
                } else if (item.icon) {
                    icon = {tag: 'span', class: item.icon, style: {'font-size': 12, 'padding-right': '0.5rem'}};
                }
                var opts = {tag: 'div',
                            class: ['menu-item'],
                            on: {click: (ev)=> { insert_item(ctx,item); } },
                            els: {icon:  icon,
                                  name:  {tag: 'b', style: {'padding-right': 5}, text: item.name},
                                  title: {tag: 'span', text: item.title}},
                            style: {padding: 5, 'font-size': 12, cursor: 'point'}};
                if(i == 0) { opts.style = merge(opts.style, selection_style); }
                item_els[i] = opts;
            }
            set(els.pop, {style: {display: 'block', top: cur.offsetTop + 18, left: cur.offsetLeft, height: 500, 'overflow-y': 'auto'},
                          els: item_els});

        } else {
            hide_popup(ctx);
        }
    } else {
        hide_popup(ctx);
    }};


var get_in = (obj, path) => {
    var res = obj;
    for (var i of path) {
        res = res[i];
        if(!res) { return res; }
    }
    return res;
};


var select = (ctx, dir) => {
    var textarea = ctx.editor.els.popup;
    var sel = ctx.selection;
    var len = ctx.items.length;
    if(sel == 0 && dir < 0) {
        sel = len -1;
    } else if (sel > (len - 2) && dir > 0) {
        sel = 0;
    } else if (sel <  len ) {
        sel = sel + dir;
    }
    set(get_in(ctx, ['editor', 'els', 'pop', 'els', ctx.selection]), {style: {background: 'transparent'}});
    set(get_in(ctx, ['editor', 'els', 'pop', 'els', sel]), {style: selection_style, intoView: true});


    ctx.selection = sel;
};

var stop = (ev) => {
    ev.preventDefault();
    ev.stopPropagation();
    ev.stopImmediatePropagation();
};

var save = (ctx)=>{

    var value = ctx.editor.els.textarea.value;
    var doc = ctx.doc;
    fetch(`/${doc}`, {method: 'POST', body: value}).then((resp)=> {
        window.location.href = `/${doc}`;
    });
}

var cancel = (ctx)=>{
    var doc = ctx.doc;
    window.location.href = `/${doc}`;
}

var on_editor_keydown = (ctx, ev) => {
    if(ev.ctrlKey && ev.key == 's') {
        save(ctx);
        stop(ev);
        return false;
    } else if (ev.ctrlKey && ev.key == 'x') {
        cancel(ctx);
    }
    if(ctx.items) {
        if(ev.key == "Enter") {
            stop(ev);
            insert_selection(ctx);
            ctx.skip_up = true;
            return false;
        } else if (ev.key == 'ArrowUp' || (ev.key == 'p' && ev.ctrlKey)) {
            stop(ev);
            select(ctx,-1);
            ctx.skip_up = true;
            return false;
        } else if (ev.key == 'ArrowDown' || (ev.key == 'n' && ev.ctrlKey)) {
            stop(ev);
            select(ctx,+1);
            ctx.skip_up = true;
            return false;
        } else if ((ev.key == 'c' && ev.ctrlKey)) {
            hide_popup(ctx);
        }
    }
}

var on_editor_keyup = (ctx, ev) => {
    if(ctx.skip_up) {
        ctx.skip_up = false;
        return;
    }
    var t = ev.target;
    var v = t.value;
    if( v !== ctx.prev_value ){
        ctx.prev_value = v;
        auto_close(ev.key, t);
        hl(ctx, v);
        autocompl(ctx, v);
    }
}


var poss = {display: 'block',
            border: none,
            outline: none,
            background: transparent,
            "font-size": 14,
            padding: 10,
            margin: 0,
            'line-height': 20,
            "overflow-wrap": 'break-word',
            "white-space": 'pre-wrap',
            "font-family": 'monospace'};

var popup_style = {position: absolute,
                   display: none,
                   'box-shadow': '1px 2px 2px #ddd',
                   'min-width': '10em',
                   'border-radius': 5,
                   background: black,
                   border: '1px solid #ddd'};


var caret_style  = merge(poss, {color: transparent,
                                overflow: 'hidden',
                                position: absolute, margin: 0, top: 0, left: 0});

var textarea_style = merge(poss, {overflow: 'hidden', position: absolute, margin: 0, top: 0, left: 0});

var editor = (zendoc) => {
    var symIdx = new quickScore.QuickScore(zendoc.symbols, ["name", "title"]);
    var keysIdx = new quickScore.QuickScore(zendoc.keys, ["name", "title"]);
    var iconsIdx = new quickScore.QuickScore(zendoc.icons, ["name", "title"]);
    var ctx = {symbols: symIdx, keys: keysIdx, icons: iconsIdx, doc: zendoc.doc};
    var in_chrome = (window.location.search || '').includes('chrome') || document.body.getBoundingClientRect().width < 800;

    var editor_style = {position: relative ,
                        background: black,
                        color: 'white',
                        overflow: 'auto', width: in_chrome ? '100vw' : '40vw',
                        height: 'calc(100vh)'};

    ctx.container = el({tag: 'div',
                        append: document.body,
                        style: {display: 'flex', padding: 0},
                        els: {save: {tag: 'i',
                                     class: ['fa-solid', 'fa-save'],
                                     style: { position: absolute,
                                              cursor: 'pointer',
                                              'z-index': "10000",
                                              bottom: 10, left: in_chrome ? 'calc(100vw - 50px)' : 'calc(40vw - 50px)',
                                              'font-size': 40, color: 'white'},
                                     on: {click: (ev)=> { save(ctx); }}}}
                       });

    var keypress = (ev)=>  {
        fetch('/preview', {method: 'POST', body: ev.target.value}).then((resp)=> {
            resp.text().then((txt)=> {
                ctx.preview.innerHTML = txt;
            });
        });
    };

    ctx.editor = el({tag: 'div',
                     append: ctx.container,
                     style: editor_style,
                     els: {hl:    {tag: 'pre', style: merge(poss,{})},
                           caret: {tag: 'pre', style: caret_style,
                                   els: {txt: {tag: 'text'},
                                         cursor: {tag: 'span', text: ' ', style: {}}}},
                           textarea: {tag: 'textarea',
                                      spellcheck: false,
                                      autofocus: true,
                                      on: {keyup:    (ev) => { on_editor_keyup(ctx,ev);},
                                           keydown:  (ev) => { on_editor_keydown(ctx ,ev); },
                                           keypress: ! in_chrome ? debounce(keypress, 300) : ()=> {},
                                           click:    (ev) => { hide_popup(ctx); }},
                                      value: zendoc.text || "",
                                      style: textarea_style},
                           pop: {tag: 'div', style: popup_style }}});
    hl(ctx, zendoc.text);
    if(! in_chrome) {
        ctx.preview = el({tag: 'div', html: zendoc.preview,
                          style: {height: 'calc(100vh - 20px)',
                                  padding: 20,
                                  width: '60vw',
                                  overflow: 'auto'},
                          append: ctx.container});
    }
    return ctx;
};

main(()=>{
    if(window.zendoc){
        editor(zendoc);
    }
});
