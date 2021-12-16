

const state = {};
const currentPathElms = document.location.pathname.slice(1).split(".")

currentPathElms.forEach((_, idx) => {
  document.querySelector(currentPathElms.slice(0, idx + 1)
                                        .map(elm => "#" + elm)
                                        .join(" "))
          .classList.remove("closed")

})

console.log(currentPathElms)
