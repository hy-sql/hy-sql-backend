/** Removes commas from the given array.
 * @param {Array} stringArray a string array
 */
const cleanStringArray = (stringArray) => {
    return stringArray.map((col) => col.replace(/,/g, '').trim())
}

/** Maps the given array into an array of objects that contain the original
 *  values as values of name-keys.
 * @param {Array} stringList a string array
 */
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
