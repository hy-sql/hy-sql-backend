const { parseConditions } = require('./fieldParser')

/**
 * Parses the WHERE part of a command into a WHERE object from the given string array.
 * @param {string[]} slicedCommandAsStringArray the WHERE part of a command as string array
 */
const parseWhere = (slicedCommandAsStringArray) => {
    const indexOfOrder = slicedCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'ORDER'
    )

    const parsedWherePart = {
        keyword: slicedCommandAsStringArray[0],
        conditions: parseConditions(
            slicedCommandAsStringArray.slice(
                1,
                indexOfOrder >= 0
                    ? indexOfOrder
                    : slicedCommandAsStringArray.length
            )
        ),
    }

    return parsedWherePart
}

module.exports = { parseWhere }
