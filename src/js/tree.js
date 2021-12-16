const openedNodesIds = Object.keys(window.sessionStorage);

console.log(openedNodesIds);

openedNodesIds.forEach(id =>
  document
    .getElementById(id)
    .getElementsByClassName("closableContent")[0]
    .classList
    .remove("closed"))

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
