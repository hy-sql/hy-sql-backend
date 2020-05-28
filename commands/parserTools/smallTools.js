const cleanStringArray = (stringArray) => {
    return stringArray.map((col) => col.replace(/,/g, '').trim())
}

const namify = (stringList) => {
    return stringList.map((str) => {
        return {
            name: str,
        }
    })
}

module.exports = {
    cleanStringArray,
    namify,
}
