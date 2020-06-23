const SQLError = require('../../models/SQLError')

/**
 * Checks whether the LIMIT keyword is correctly positioned in regards to other currently
 * existing keywords (so after WHERE, GROUP BY and ORDER BY). Also checks that the keyword
 * OFFSET is not found before keyword LIMIT. Throws an error if not correctly positioned.
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

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    if (indexOfLimit < indexOfWhere) {
        correctlyPositioned = false
    }

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    if (
        indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY' &&
        indexOfLimit < indexOfGroup
    ) {
        correctlyPositioned = false
    }

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (
        indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfLimit < indexOfOrder
    ) {
        correctlyPositioned = false
    }

    if (!correctlyPositioned) {
        throw new SQLError(
            'OFFSET must always be after LIMIT and LIMIT can not be before WHERE, GROUP BY or ORDER BY'
        )
    }
}

module.exports = { checkLimitPosition }
