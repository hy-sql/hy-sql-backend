/**
 * Checks whether the LIMIT keyword is correctly positioned in regards to other currently
 * existing keywords (so after WHERE, GROUP BY and ORDER BY). Also checks that the keyword
 * OFFSET is not found before keyword LIMIT.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} true/false: LIMIT keyword is correctly positioned
 */
const checkLimitPosition = (fullCommandAsStringArray) => {
    const indexOfLimit = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'LIMIT'
    )

    const indexOfOffset = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'OFFSET'
    )
    if (indexOfOffset !== -1 && indexOfOffset < indexOfLimit) return false

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )
    if (indexOfLimit < indexOfWhere) return false

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )
    if (
        indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY' &&
        indexOfLimit < indexOfGroup
    ) {
        return false
    }

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )
    if (
        indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfLimit < indexOfOrder
    ) {
        return false
    }

    return true
}

module.exports = { checkLimitPosition }
