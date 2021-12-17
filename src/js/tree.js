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
    const leftPart = wordIndex > 201 ? text.slice(wordIndex - 100, wordIndex) : text.slice(0, wordIndex)
    const rightPart = text.slice(wordIndex, wordIndex + 50)
    const nodeText = leftPart
          .concat(rightPart)
          .toLowerCase()
          .replaceAll(word, `<span class=bolder>${word}</span>`)
    return `<p>${nodeText}</p>`
}

const searchHandler = (e) =>
      {
          const searchValue = e.target.value.toLowerCase()
          const foundResources =
                Object
                .entries(searchData)
                .map(v =>
                    {
                        const d = v[1];
                        if (d.summary && d.summary.includes(searchValue))
                        {return [v[0], {title: d.title,
                                        focusedSummary: focusOnWordWithSurroundings(searchValue, d.summary)}]}
                        else if (d.title.includes(searchValue) || d.kpath.includes(searchValue))
                        {return [v[0], {title: d.title}]}
                    })
                .filter(x => x)
          removeChildElms(document.getElementById("searchResults"))
          foundResources.forEach(res =>
              {
                  let kpath, r;
                  [kpath, r] = res;
                  const link = document.createElement("a")
                  link.setAttribute("href", kpath)
                  link.classList.add("searchResultContainer")
                  const wrapperDiv = document.createElement("div")
                  wrapperDiv.classList.add("searchResultContainerRow")
                  const vBar = document.createElement("div")
                  vBar.classList.add("searchResultContainerVBar")
                  wrapperDiv.appendChild(vBar)

                  const title = document.createElement("span")
                  title.classList.add("searchResultTitle")
                  title.appendChild(document.createTextNode(capitalize(r.title)))

                  wrapperDiv.appendChild(title)
                  link.appendChild(wrapperDiv)

                  if (r.focusedSummary)
                  {   const summaryText = new DOMParser()
                            .parseFromString(r.focusedSummary, 'text/html')
                            .body
                            .firstElementChild
                      link.appendChild(summaryText) }

                  document
                      .getElementById("searchResults")
                      .appendChild(link)})}

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
    .getElementById("overlay")
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

            document
                .getElementById("searchInput")
                .focus()

        })

document.onkeyup = (e) => {
    if (e.altKey && (e.code === "KeyK"))
    {

        e.stopPropagation()
        document
            .getElementById("searchContainer")
            .classList
            .add("visible")


        document
            .getElementById("overlay")
            .classList
            .add("visible")


        document
            .getElementById("searchInput")
            .focus()
    }

}


//Tree navigation stuff
const openedNodesIds = Object.keys(window.sessionStorage);


openedNodesIds.forEach(id => {
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

                          }));

const tabs = Array.from(document.getElementsByClassName("tab"));
var currentTab = null;
var currentTabCnt = null;
tabs.forEach( node => {
    if(node.classList.contains("active-nav")){
        currentTab = node;
        currentTabCnt = document.getElementById(node.getAttribute("for"));
    }
    node.addEventListener("click",
                          (e) => {
                              e.preventDefault();
                              e.stopPropagation();
                              var el = e.target;
                              if(el == currentTab) {return;}
                              currentTabCnt && (currentTabCnt.style.display = "none");
                              currentTab && currentTab.classList.toggle("active-nav");

                              var cnt = document.getElementById(el.getAttribute("for"));
                              cnt && (cnt.style.display = "block");
                              el.classList.toggle("active-nav");
                              currentTabCnt = cnt;
                              currentTab = el;

                          });
});
