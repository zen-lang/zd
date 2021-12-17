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

const capitalize = (str) => str
      .split(" ")
      .map(word => (word.charAt(0).toUpperCase() + word.slice(1)))
      .join(" ")

const focusOnWordWithSurroundings = (word, text) => {
    const wordIndex = text.indexOf(word)
    const leftPart = text.slice(0, wordIndex)
    const rightPart = text.slice(wordIndex, wordIndex + 50)
    return leftPart + rightPart
}

const searchHandler = (e) =>
      {const searchValue = e.target.value
       const foundResources =
             Object
             .entries(searchData)
             // .map(v =>
             //     {
             //         const d = v[1];
             //         if d.summary.includes(searchValue)
             //         {return {indexOf: d.summary.indexOf(searchValue)
             //                  focusedSummary: d.summary.slice()
             //                 }}
             //     })
             .filter((v) =>
                 v[1].title.includes(searchValue)
                     || v[1].summary.includes(searchValue))
       removeChildElms(document.getElementById("searchResults"))
       console.log(foundResources);
       foundResources.forEach(res =>
           {
               let kpath, r;
               [kpath, r] = res;
               const div = document.createElement("div")
               div.classList.add("searchResultContainer")
               const vBar = document.createElement("div")
               vBar.classList.add("searchResultContainerVBar")
               div.appendChild(vBar)

               const link = document.createElement("a")
               link.setAttribute("href", kpath)
               link.classList.add("searchResultTitle")
               link.appendChild(document.createTextNode(capitalize(r.title)))

               const wrapperDiv = document.createElement("div")
               wrapperDiv.appendChild(link)

               div.appendChild(wrapperDiv)

               document
                   .getElementById("searchResults")
                   .appendChild(div)})}

document
    .getElementById("searchInput")
    .addEventListener("input", throttle(searchHandler, 300))



document
    .getElementById("searchContainerClose")
    .addEventListener("click", (_e) =>
        {
            document
                .getElementById("searchContainer")
                .classList
                .remove("visible")


            document
                .getElementById("overlay")
                .classList
                .remove("visible")

        })

document
    .getElementById("searchButton")
    .addEventListener("click", (_e) =>
        {
            document
                .getElementById("searchContainer")
                .classList
                .add("visible")


            document
                .getElementById("overlay")
                .classList
                .add("visible")

        })


//Tree navigation stuff
const openedNodesIds = Object.keys(window.sessionStorage);


openedNodesIds.forEach(id => {
    console.log(id);
    document
        .getElementById(id)
        ?.getElementsByClassName("closableContent")[0]
        ?.classList
        .toggle("closed");
});

openedNodesIds.forEach(id =>
    document
        .getElementById(id)
        ?.getElementsByClassName("toggler")[0]
        ?.classList
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

                              const isClosed = closableNode?.classList.contains("closed");

                              isClosed ?
                                  window.sessionStorage.setItem(closableNodeParent.id, null)
                                  :
                                  window.sessionStorage.removeItem(closableNodeParent.id, null)

                              closableNode.classList.toggle("closed")
                              node.classList.toggle("rotateToggler")

                          }))
