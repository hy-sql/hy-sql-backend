/**
 * Searches for the index of the closing bracket that matches the opening one
 * at the given index.
 * @param {string[]} conditionsArray array of command conditions
 * @param {Number} IndexOfOpeningBracket index of opening bracket
 * @returns {Number} index of the matching closing bracket
 */
const findIndexOfClosingBracket = (conditionsArray, IndexOfOpeningBracket) => {
    const brackets = { opening: 0, closing: 0 }

    for (let i = IndexOfOpeningBracket; i < conditionsArray.length; i++) {
        if (conditionsArray[i] === '(') {
            brackets.opening = brackets.opening + 1
        } else if (conditionsArray[i] === ')') {
            brackets.closing = brackets.closing + 1
        }

        if (brackets.opening === brackets.closing) {
            return i
        }
    }
}

module.exports = findIndexOfClosingBracket
