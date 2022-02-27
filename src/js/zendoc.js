document.addEventListener('DOMContentLoaded', function(){
    Array.prototype.slice.call(document.getElementsByClassName("zd-toggle")).map(function(el){
        el.querySelector('.zd-block-title').addEventListener("click", function () {
            el.classList.toggle("zd-open");
            console.log(el);
        });
    });

});
