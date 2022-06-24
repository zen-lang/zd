function toa(x){
    return Array.prototype.slice.call(x);
}

document.addEventListener('DOMContentLoaded', function(){
    toa(document.getElementsByClassName("zd-toggle")).map(function(el){
        el.querySelector('.zd-block-title').addEventListener("click", function () {
            el.classList.toggle("zd-open");
            console.log(el);
        });
    });

    toa(document.getElementsByClassName("zd-tabs")).map(function(el){
        var activetab = el.querySelector('.tabh.active');
        var id = activetab && activetab.getAttribute('for');
        var current = id && document.getElementById(id);
        console.log('current', activetab, current);

        el.querySelector('.tabh').addEventListener("click", function (ev) {
            console.log(ev);
            if(! ev.target.classList.contains("tabh")){
                return;
            }
            var id = ev.target.getAttribute('for');
            var node = id && document.getElementById(id);
            if (node) {
                node.style.display = "block";
            }
            if(current && current !== node){
               current.style.display = "none";
               activetab && activetab.classList.toggle("active");
               ev.target.classList.toggle("active");
               activetab = ev.target;
               current = node;
            }

        });

    });

    toa(document.getElementsByClassName("code-block")).map(function(el) {
        el.querySelector(".copy-button").addEventListener("click", function () {
            copyToClipboard(el.querySelector("pre").querySelector("code").innerText);
        })
    });
});

function on_form_validate() {
    form_element = document.getElementById("form-validate");
    form_element.submit();
}

function copyToClipboard(str) {
  var area = document.createElement('textarea');

  document.body.appendChild(area);
    area.value = str;
    area.select();
    document.execCommand("copy");
  document.body.removeChild(area);
}
