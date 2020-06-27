/**
 * Searches for the index of the closing bracket that matches the opening one
 * at the given index.
 * @param {string[]} stringArray array of command conditions
 * @param {Number} IndexOfOpeningBracket index of opening bracket
 * @returns {Number} index of the matching closing bracket
 */
const findIndexOfClosingBracket = (stringArray, IndexOfOpeningBracket) => {
    const brackets = { opening: 0, closing: 0 }

    for (let i = IndexOfOpeningBracket; i < stringArray.length; i++) {
        if (stringArray[i] === '(') {
            brackets.opening = brackets.opening + 1
        } else if (stringArray[i] === ')') {
            brackets.closing = brackets.closing + 1
        }

        if (brackets.opening === brackets.closing) {
            return i
        }
    }
}

module.exports = findIndexOfClosingBracket
