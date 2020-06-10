const findIndexOfClosingBracket = (conditionsArray, IndexOfOpeningBracket) => {
    let brackets = { opening: 0, closing: 0 }

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
