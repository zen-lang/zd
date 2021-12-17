//Search container stuff
function throttle(func, ms) {

  let isThrottled = false,
    savedArgs,
    savedThis;

  function wrapper() {

    if (isThrottled) { // (2)
      savedArgs = arguments;
      savedThis = this;
      return;
    }

    func.apply(this, arguments); // (1)

    isThrottled = true;

    setTimeout(function() {
      isThrottled = false; // (3)
      if (savedArgs) {
        wrapper.apply(savedThis, savedArgs);
        savedArgs = savedThis = null;
      }
    }, ms);
  }

  return wrapper;
}

const removeChildElms = (node) =>
      {while (node.firstChild)
       {node.removeChild(node.lastChild)}}

const searchHandler = (e) =>
      {const searchValue = e.target.value
       const foundResources =
             Object
             .entries(searchData)
             .filter((v) =>
               v[1].title.includes(searchValue)
                 || v[1].summary.includes(searchValue))
       removeChildElms(document.getElementById("searchResults"))
       console.log(foundResources);
       foundResources.forEach(res =>
         {
           let kpath, r;
           [kpath, r] = res;
           const link = document.createElement("a")
           link.setAttribute("href", kpath)
           link.classList.add("searchResult")
           link.appendChild(document.createTextNode(r.title))

           document
             .getElementById("searchResults")
             .appendChild(link)})}

document
  .getElementById("searchInput")
  .addEventListener("input", throttle(searchHandler, 300))

document
  .getElementById("searchContainerClose")
  .addEventListener("click", (_e) =>
    document
      .getElementById("searchContainer")
      .classList
      .remove("visible"))

document
  .getElementById("searchButton")
  .addEventListener("click", (_e) =>
    document
      .getElementById("searchContainer")
      .classList
      .add("visible"))


//Tree navigation stuff
const openedNodesIds = Object.keys(window.sessionStorage);


openedNodesIds.forEach(id =>
  document
    .getElementById(id)
    .getElementsByClassName("closableContent")[0]
    .classList
    .toggle("closed"))

openedNodesIds.forEach(id =>
  document
    .getElementById(id)
    .getElementsByClassName("toggler")[0]
    .classList
    .toggle("rotateToggler"))

const currentPathElms = document.location.pathname.slice(1).split(".")

currentPathElms.forEach((_, idx) => {
  document
    .querySelector(currentPathElms.slice(0, idx + 1)
                   .map(elm => "#" + elm)
                   .join(" "))
    .classList.remove("closed")

})

const togglerElms = Array.from(document.getElementsByClassName("toggler"));

togglerElms.forEach(node =>
  node.addEventListener("click",
                        (e) => {
                          e.preventDefault();
                          e.stopPropagation();
                          const closableNodeParent = node.closest(".closable")
                          const closableNode =
                                closableNodeParent
                                .getElementsByClassName("closableContent")[0];

                          const isClosed = closableNode.classList.contains("closed");

                          isClosed ?
                            window.sessionStorage.setItem(closableNodeParent.id, null)
                            :
                            window.sessionStorage.removeItem(closableNodeParent.id, null)

                          closableNode.classList.toggle("closed")
                          node.classList.toggle("rotateToggler")

                        }))

