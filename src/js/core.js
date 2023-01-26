const debounce = (func, delay) => {
    let debounceTimer;
    return function() {
        const context = this;
        const args = arguments;
        clearTimeout(debounceTimer);
        debounceTimer = setTimeout(() => func.apply(context, args), delay);
    };
};

var merge = (a,b) => {
    return Object.assign({}, a,b);
};
var _id = (id) => {
    return document.getElementById(id);
};

var stop = (ev) => {
    ev.preventDefault();
    ev.stopPropagation();
    ev.stopImmediatePropagation();
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
    if(attrs.class) { attrs.class.forEach((c)=>{ if(c) { nel.classList.add(c); }}); }
    if(attrs.text) { nel.innerText = attrs.text; }
    if(attrs.html) { nel.innerHTML = attrs.html; }
    if(attrs.href) { nel.href = attrs.href; }
    if(attrs.src) { nel.src = attrs.src; }
    if(attrs.data) {
        for(var k in attrs.data) {
            var v = attrs.data[k];
            nel.dataset[k] = v;
        }
    }
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

var load_page = (href, do_push)=>{
    fetch(href,{headers: {'x-body': 'true', 'cache-control': 'no-cache'}}).then((res)=>{
        console.log('redirect?', res);
        if(res.redirected){
            window.location.href = res.url;
        } else {
            res.text().then((txt)=>{
                if(do_push){
                    window.history.pushState({href: href}, '', href);
                }
                document.getElementById('page').innerHTML= txt;
                update_widgets();
            });
        }
    });
}

var get_href = (ev)=>{
    var target = ev.target;

    var href = null;
    while(target){
        if(target.href){
            href = target.href;
            break;
        } else {
            target = target.parentNode;
        }
    }
    return href;
}

var on_menu_click = (ev)=>{
    // console.log(ev.target.href);
    var href = get_href(ev);
    load_page(href,true);
    ev.preventDefault();
    ev.stopPropagation();
    ev.stopImmediatePropagation();
    return false;
};


var get_icon = (v) => {
    var icon = null;
    if(v.icon){
        icon = {tag: 'i', class: v.icon};
    } else if (v.logo) {
        icon = {tag: 'img', src: v.logo, style: {width: '1rem', height: '1rem', display: 'inline-block', 'border-radius': '100%'}};
    } else {
        icon = {tag: 'i', class: ['fa-solid', 'fa-file']};
    }
    return icon;
};



var render_menu = (sym)=> {
    var menu = document.getElementById('aside');
    if(!menu){
        return;
    }
    var res = zd.nav.resources[sym] || zd.nav;
    var items = [];

    if(res.items){
        items = res.items;
    }
    // else {
    //     sym = sym.split('.').slice(0,-1).join('.');
    //     res = zd.nav.resources[sym] || zd.nav;
    //     items = res.items;
    // }

    // if( sym && sym == zd.current) {
    //     console.log('same',sym,zd.current);
    //     return;
    // }
    // zd.current = sym;

    localStorage.setItem('zd/nav',sym);

    items = items.map((x)=> {
        return zd.nav.resources[x];
    });

    var els = {};
    if(sym){
        var parent = sym.split('.').slice(0,-1).join('.');
        var parent_doc = zd.nav.resources[parent] || {};
        els.back = {tag: 'a',
                    on: {click: (ev)=> { render_menu(parent); }},
                    style: {'border-bottom': '1px solid #ddd', 'padding': '0.5rem 1rem'},
                    class: ['zd-menu-item'],
                    els: {icon: {tag: 'i', class: ['fa-duotone', 'fa-folder-arrow-up']},
                          text: {tag: 'span', text: (parent_doc.title || parent || 'Home')}}};
        els.root = {tag: 'a',
                    style: {'padding': '0.5rem 1rem'},
                    href: `/${sym}`,
                    class: ['zd-menu-item'],
                    els: {icon: {tag: 'i', class: ['fa-duotone', 'fa-folder']},
                          text: {tag: 'span', text: sym}}};
    }
    for(var v of items){
        if(v){
            var icon = get_icon(v);

            var folder = null;
            if( v.items ) {
                folder = {tag: 'a',
                          data: {dir: v.name},
                          href: `/${v.name}`,
                          els: {icon: {tag: 'i', class: ['zd-folder', 'fa-solid', 'fa-folder'], style: {} }}};
            } else {
                folder = {tag: 'a',
                          href: `/${v.name}`,
                          data: {dir: v.name},
                          els: {icon: {tag: 'i', class: ['zd-folder', 'zd-empty', 'fa-solid', 'fa-folder']}}};
            }

            var name_parts = v.name.split('.');
            var local_name = name_parts[name_parts.length - 1];
            els[v.name] = {tag: 'a',
                           class: ['zd-menu-item'],
                           // on: { click: on_menu_click, dblclick: on_menu_dbl_click},
                           style: {},
                           href: `/${v.name}`,
                           els: {icon: icon,
                                 text: {tag: 'span',
                                        text: v.title || local_name,
                                        style: {flex: '1'}},
                                 folder: folder}};
        }
    }
    menu.innerHTML = '';
    el({tag: 'div', style: {}, append: menu, els: els});
};
var href_sym =()=>{
    var href = window.location.href;
    return href.split('/')[3];
};


var get_href = (ev)=>{
    var target = ev.target;

    var res = {};
    while(target){
        if(target.href){
            res.href = target.href;
            res.dir = target.dataset.dir;
            break;
        } else {
            target = target.parentNode;
        }
    }
    return res;
};

var on_link_click = (ev)=>{
    var res = get_href(ev);
    try {
        if(res.dir) {
            render_menu(res.dir);
            ev.preventDefault();
            ev.stopPropagation();
            ev.stopImmediatePropagation();
            return false;
        } else if(res.href){
            var href = res.href;
            var parts = href.split('/');
            if(parts.length == 4 && parts[2] == 'localhost:3333') {
                load_page(href,true);
                ev.preventDefault();
                ev.stopPropagation();
                ev.stopImmediatePropagation();
                return false;
            }
        }
    } catch (e) {
        console.log(e);
        ev.preventDefault();
        ev.stopPropagation();
        ev.stopImmediatePropagation();
        return false;
    }
};

var on_link_dblclick = (ev)=> {
    var href = get_href(ev);
    if(href){
        var parts = href.split('/');
        if(parts.length == 4 && parts[2] == 'localhost:3333') {
            load_page(href,true);
            render_menu(parts[3]);
            ev.preventDefault();
            ev.stopPropagation();
            ev.stopImmediatePropagation();
            return false;
        }
    }
};



var on_search =(popup, ev) =>{
    var q = ev.target.value;
    var res = {};
    var items = zd.searchIdx.search(q);
    for( var i = 0; i < items.length; i++ ){
        var x  = items[i];
        var item = x.item;
        res[i] = {tag: 'a',
                  class: ['zd-search-item', (i == 0) ? 'current' : null],
                  on: {click: (ev)=>{ popup.remove(); }},
                  href: `/${item.name}`,
                  data: {dir: item.name},
                  els: {icon: get_icon(item),
                        text: {tag: 'div',
                               class: ['zd-search-title'],
                               text: item.title || item.name},
                        desc: {tag: 'div',
                               class: ['zd-search-desc'],
                               text: item.name}}};
    }
    var res_node = popup.els.results;
    res_node.innerHTML = '';
    set(res_node, {els: res});
    zd.popup.current = 0;
    zd.popup.items = items;
}

var search_selection = (i) => {
    if(! zd.popup ) { return ;}
    var index = zd.popup.current + i;
    var len = zd.popup.items.length - 1;
    var res_node = zd.popup.els.results.els;
    if(index < 0) {
        index = len;
    } else if (index > len) {
        index = 0;
    }
    var cur_node = res_node[zd.popup.current];
    if(cur_node){
        cur_node.classList.remove('current');
    }
    cur_node = res_node[index];
    if(cur_node){
        cur_node.classList.add('current');
        cur_node.scrollIntoView({
            behavior: "smooth",
            block: "end",
            inline: "nearest"
        });

    }
    zd.popup.current = index;
}

var open_search_selection = (with_dir)=>{
    var res_node = zd.popup.els.results.els;
    var cur_node = res_node[zd.popup.current];
    var href = cur_node && cur_node.href;
    load_page(href,true);
    if(with_dir){
        render_menu(cur_node.dataset.dir);
    }
    close_search({});
};

var on_search_key = (ev) => {
    if(ev.key == "Enter") {
        open_search_selection(ev.ctrlKey);
        stop(ev);
        return false;
    } else if (ev.key == 'ArrowUp' || (ev.key == 'p' && ev.ctrlKey)) {
        search_selection(-1);
        stop(ev);
        return false;
    } else if (ev.key == 'ArrowDown' || (ev.key == 'n' && ev.ctrlKey)) {
        search_selection(1);
        stop(ev);
        return false;
    }
};

var open_search = (ev)=> {
    var popup = el({tag: 'div',
                    class: ['zd-search-popup'],
                    on: {click: close_search},
                    append: document.body,
                    els: {search: {tag: 'input',
                                   class: ['zd-search'],
                                   on: {input: (ev)=> { on_search(popup, ev); },
                                        keydown: on_search_key}},
                          comments: {tag: 'div', class: ['zd-comments'],
                                     text: '[ctrl-x] to close, [ctrl-p/n] or arrows for selection; [enter] to open; [ctrl-enter] to open with navigation'},
                          results: {tag: 'div', class: ['zd-results']}}});
    zd.popup = popup;
    popup.els.search.focus();
};

var close_search = (ev)=>{
    var popup = zd.popup;
    if(popup) {
        popup.remove();
    }
}

var on_hotkey = (e)=>{
    if ((e.ctrlKey || e.altKey || e.key === "Meta") && (e.code === "KeyK")) {
        open_search();
        e.stopPropagation();
        e.stopImmediatePropagation();
        return false;
    } else if (e.key == "Escape" || ((e.ctrlKey || e.altKey || e.key === "Meta") && (e.code === "KeyX"))) {
        close_search();
        e.stopPropagation();
        e.stopImmediatePropagation();
        return false;
    }
}

main(()=>{
    update_widgets();

    var sym = localStorage.getItem('zd/nav');
    if(sym === undefined) {
        sym =  href_sym();
    }
    render_menu(sym);

    window.addEventListener('popstate',(ev)=> {
        console.log('pop',ev.state, window.location.href);
        var href = window.location.href;
        load_page(href,false);
    });
    document.addEventListener('keydown', on_hotkey);
    document.addEventListener('click', on_link_click);
    document.addEventListener('dblclick', on_link_dblclick);
    var in_chrome = (window.location.search || '').includes('chrome') || document.body.getBoundingClientRect().width < 800;
    if(in_chrome){
        document.getElementById('left-nav').remove();
    }
    var search = document.getElementById('search');
    if(search){
        set(search, {on: {click: open_search}});
    }
    var search_items = [];
    for (var k in zd.nav.resources){
        search_items.push(zd.nav.resources[k]);
    }
    var searchIdx = new quickScore.QuickScore(search_items, ["name", "title"]);
    zd.searchIdx = searchIdx;


});
