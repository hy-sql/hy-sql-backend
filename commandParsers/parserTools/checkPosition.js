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
    if (indexOfOffset !== -1 && indexOfOffset < indexOfLimit) {
        throw new SQLError('OFFSET must always be positioned after LIMIT')
    }

    if (
        !afterWhere(fullCommandAsStringArray, indexOfLimit) ||
        !afterGroupBy(fullCommandAsStringArray, indexOfLimit) ||
        !afterOrderBy(fullCommandAsStringArray, indexOfLimit)
    ) {
        throw new SQLError(
            'LIMIT must always be positioned after WHERE, GROUP BY and ORDER BY'
        )
    }
}

const checkGroupByPosition = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    if (
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() !== 'BY'
    ) {
        return
    }

    if (
        !afterWhere(fullCommandAsStringArray, indexOfGroup) ||
        !beforeOrderBy(fullCommandAsStringArray, indexOfGroup)
    ) {
        throw new SQLError(
            'GROUP BY must always be positioned after WHERE and before ORDER BY'
        )
    }
}

/*

*/

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
        fullCommandAsStringArray[indexOfGroup + 1] &&
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
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfKeyword < indexOfOrder
        ? false
        : true
}

const beforeOrderBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    return indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfKeyword > indexOfOrder
        ? false
        : true
}

module.exports = { checkLimitPosition, checkGroupByPosition }
