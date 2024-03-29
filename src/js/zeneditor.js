function debounce(cb, timeout = 300) {
  let timer
  return (...args) => {
    window.clearTimeout(timer)
    timer = window.setTimeout(() => cb.apply(this, args), timeout)
  }
}

function syncScroll(elScrolled, elToSync) {
  const scrollPercentage = elScrolled.scrollTop / (elScrolled.scrollHeight - elScrolled.offsetHeight)
  elToSync.scrollTop = scrollPercentage * (elToSync.scrollHeight - elToSync.offsetHeight)
}

function registerScrollSync(elScrolling, elToSync) {
  elScrolling.onscroll = (e) => {
    isElScrollingNotHovered = !elScrolling.dataset.hovered
    isElToSyncFocused = document.activeElement === elToSync
    if (isElScrollingNotHovered || isElToSyncFocused) {
      return
    }

    syncScroll(elScrolling, elToSync)
  }
}

function registerHoverFlagger(el) {
  el.onmouseenter = () => el.dataset.hovered = "true"
  el.onmouseleave = () => delete el.dataset.hovered
}

function renderPreview(previewNode, editorNode) {
  const text = editorNode.value
  fetch(document.URL, { method: "POST", body: text })
    .then(response => response.text())
    .then(data => {
        previewNode.innerHTML = data
        window.requestAnimationFrame(() => syncScroll(editorNode, previewNode))
    })
}

function savePreview() {
  const editorNode = document.getElementById("edit-page")
  const text = editorNode.value

  const spinner = document.getElementById("spinner")
  spinner.classList.add("show-spinner")

  fetch(document.URL, { method: "PUT", body: text })
    .then(response => response.text())
    .then(data => location.href = data)
    .catch(() => {
      spinner.classList.remove("show-spinner")
      alert("Что то сломалось. Сори, это пока бета, можете попробовать еще раз.")
    })
}

document.addEventListener("DOMContentLoaded", function() {
  const prNode = document.getElementById("edit-preview")
  const editorNode = document.getElementById("edit-page")

  if (!prNode || !editorNode) return

  renderPreview(prNode, editorNode)
  editorNode.addEventListener("input", debounce(renderPreview, 200))

  registerHoverFlagger(prNode)
  registerHoverFlagger(editorNode)
  registerScrollSync(prNode, editorNode)
  registerScrollSync(editorNode, prNode)
})
