const SQLError = require('../../models/SQLError')

/**
 * Checks whether the LIMIT keyword is correctly positioned after keywords WHERE,
 * GROUP BY and ORDER BY. Also checks that the keyword OFFSET is not found before
 * keyword LIMIT. Throws an error if not correctly positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkLimitPosition = (fullCommandAsStringArray) => {
    const indexOfLimit = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'LIMIT'
    )

    const indexOfOffset = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'OFFSET'
    )

    let correctlyPositioned =
        indexOfOffset !== -1 && indexOfOffset < indexOfLimit ? false : true

    if (!afterWhere(fullCommandAsStringArray, indexOfLimit)) {
        correctlyPositioned = false
    }

    if (!afterGroupBy(fullCommandAsStringArray, indexOfLimit)) {
        correctlyPositioned = false
    }

    if (!afterOrderBy(fullCommandAsStringArray, indexOfLimit)) {
        correctlyPositioned = false
    }

    if (!correctlyPositioned) {
        throw new SQLError(
            'OFFSET must always be after LIMIT and LIMIT can not be before WHERE, GROUP BY or ORDER BY'
        )
    }
}

const afterWhere = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    return indexOfKeyword < indexOfWhere ? false : true
}

const afterGroupBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    return indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY' &&
        indexOfKeyword < indexOfGroup
        ? false
        : true
}

const afterOrderBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    return indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfKeyword < indexOfOrder
        ? false
        : true
}

module.exports = { checkLimitPosition }
