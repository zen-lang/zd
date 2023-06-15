// dom model

const debounce = (func, delay) => {
    let debounceTimer;
    return function() {
        const context = this;
        const args = arguments;
        clearTimeout(debounceTimer);
        debounceTimer = setTimeout(() => func.apply(context, args), delay);
    };
};

function copyToClipboard(str) {
    var area = document.createElement('textarea');

    document.body.appendChild(area);
    area.value = str;
    area.select();
    document.execCommand("copy");
    document.body.removeChild(area);
};

function toa(x){
    return Array.prototype.slice.call(x);
};

var merge = (a, b) => {
    return Object.assign({}, a, b);
};
var _id = (id) => {
    return document.getElementById(id);
};

var stop = (ev) => {
    ev.preventDefault();
    ev.stopPropagation();
    ev.stopImmediatePropagation();
};

var is_alphanum = (c) => {
    return (c || false) && c.match(/[-.a-zA-Z0-9]/i) !== null;
};

var is_in_key = (txt, sym) => {
    var line_start = txt.lastIndexOf("\n");
    var quote_count = 0;
    for (var i = txt.length - 1; i > line_start; i--) {
        if (txt[i] == '"') {
            quote_count = quote_count + 1;
        }
    }
    var lastc = txt[line_start + 1];
    return (lastc == ':' || lastc == '^') && !(quote_count & 1);
};

var set = (nel, attrs) => {
    if (!nel) {
        return nel;
    }
    if (attrs.class) {
        attrs.class.forEach((c) => {
            if (c) {
                nel.classList.add(c);
            }
        });
    }
    if (attrs.text) {
        nel.innerText = attrs.text;
    }
    if (attrs.html) {
        nel.innerHTML = attrs.html;
    }
    if (attrs.href) {
        nel.href = attrs.href;
    }
    if (attrs.src) {
        nel.src = attrs.src;
    }
    if (attrs.data) {
        for (var k in attrs.data) {
            var v = attrs.data[k];
            nel.dataset[k] = v;
        }
    }
    if (attrs.autofocus) {
        nel.autofocus = "true";
    }
    if (attrs.intoView) {
        nel.scrollIntoView({
            behavior: "smooth",
            block: "end",
            inline: "nearest"
        });
    }
    if (attrs.on) {
        for (e in attrs.on) {
            var f = attrs.on[e];
            nel.addEventListener(e, f);
        }
    }
    if (attrs.style) {
        for (s in attrs.style) {
            var v = attrs.style[s];
            if (Number.isInteger(v)) {
                v = v + 'px';
            }
            nel.style[s] = v;
        }
    }
    if (attrs.value) {
        nel.value = attrs.value;
    }
    if (attrs.spellcheck != undefined) {
        nel.spellcheck = attrs.spellcheck.toString();
    }
    if (attrs.els) {
        nel.els = {};
        nel.innerHTML = '';
        for (id in attrs.els) {
            var def = attrs.els[id];
            if (def) {
                def.append = nel;
                nel.els[id] = el(def);
            }
        }
    }
    if (attrs.append) {
        attrs.append.appendChild(nel);
    }
    return nel;
};

var el = (attrs) => {
    var nel = null;
    if (attrs.tag === 'text') {
        nel = document.createTextNode("");
    } else {
        nel = document.createElement(attrs.tag);
    }
    set(nel, attrs);
    return nel;
};

var main = (f) => {
    document.addEventListener('DOMContentLoaded', () => {
        f();
    }, false);
};

// zd js
var update_widgets = () => {
    for (var el of document.getElementsByClassName("widget")) {
        var url = el.dataset.url;
        if (url) {
            el.innerHTML = "<i class='fa fa-spinner fa-spin'></i>";
            fetch(url, {
                headers: {
                    'x-client-path': window.location.pathname,
                    'x-client-qs': window.location.search,
                    'x-client-hash': window.location.hash
                }
            }).then((resp) => {
                window.resp = resp;
                if (resp.status < 300) {
                    resp.text().then((t) => {
                        el.innerHTML = t;
                    });
                } else {
                    console.log('ups', resp);
                    el.innerHTML = `Error ${resp.status}`;
                }
            });
        }
    }
};

var load_page = (href, do_push) => {
    fetch(href, {
        headers: {
            'x-body': 'true',
            'cache-control': 'no-cache'
        }
    }).then((res) => {
        if (res.redirected) {
            window.location.href = res.url;
        } else {
            res.text().then((txt) => {
                if (do_push) {
                    const new_href = href + window.location.search;
                    window.history.pushState({
                        href: new_href
                    }, '', new_href);
                }
                document.getElementById('page').innerHTML = txt;
                update_widgets();
            });
        }
    });
}

var href_sym = () => {
    var href = window.location.href;
    return href.split('/')[3];
};

var get_href = (ev) => {
    var target = ev.target;

    var res = {};
    while (target) {
        if (target.href) {
            res.href = target.href;
            res.dir = target.dataset.dir;
            break;
        } else {
            target = target.parentNode;
        }
    }
    return res;
};

var on_link_click = (ev) => {
    var res = get_href(ev);
    try {
        if (res.href) {
            var l = new URL(res.href);
            var href = res.href;
            var parts = href.split('/');
            if (!l.hash && parts.length == 4) {
                load_page(href, true);
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

var open_search = (ev) => {};

var close_search = (ev) => {};

var on_hotkey = (e) => {
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
};

var search_debounce = debounce((v) => {
    var searchString = window.location.search;

    var searchParams = new URLSearchParams(searchString);

    if (v.length == 0) {
        searchParams.delete('search');
        searchParams.delete('page');
    } else {
        searchParams.set('search', v);
    }
    window.location.search = searchParams.toString();
}, 1200);

var on_search_input = (e) => {
    search_debounce(e.target.value);
}

// TODO implement create by backlink flow
var create_redirect = (e) => {
    var el = document.getElementById("zd-select");
    var p = window.location.pathname.substring(1);
    window.location.href = p + "._draft/edit?" + window.location.search;
};

main(() => {
    update_widgets();

    // TODO init tabs only if current page is not editor

    // TODO move to nav widget init, use dom model

    document.getElementById('zd-search-input').addEventListener('input', on_search_input);

    var sp = new URLSearchParams(window.location.search);

    var search_input = document.getElementById('zd-search-input');

    if (sp.get('search')){
        search_input.focus();
        search_input.selectionStart = search_input.selectionEnd = search_input.value.length;
    }

    window.addEventListener('popstate', (ev) => {
        console.log('pop', ev.state, window.location.href);
        var href = window.location.href;
        load_page(href, false);
    });
    // TODO check what prefix is used in browser apps
    document.addEventListener('keydown', on_hotkey);
    document.addEventListener('click', on_link_click);

    var in_chrome = (window.location.search || '').includes('chrome') || document.body.getBoundingClientRect().width < 800;
    if (in_chrome) {
        document.getElementById('left-nav').remove();
    }

    toa(document.getElementsByClassName("zd-toggle")).map(function(el){
        el.querySelector('.zd-block-title').addEventListener("click", function () {
            el.classList.toggle("zd-open");
        });
    });

    toa(document.getElementsByClassName("code-block")).map(function(el) {
        el.querySelector(".copy-button").addEventListener("click", function () {
            copyToClipboard(el.querySelector("pre").querySelector("code").innerText);
        });
    });

});
