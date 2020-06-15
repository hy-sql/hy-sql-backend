const { parseConditions } = require('./fieldParser')

/** Parses the WHERE part of a command into a WHERE object from the given string array.
 * @param {string[]} slicedCommandAsStringArray the WHERE part of a command as string array
 */
const parseWhere = (slicedCommandAsStringArray) => {
    const indexOfOrderBy = slicedCommandAsStringArray.find(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const parsedWherePart = {
        keyword: slicedCommandAsStringArray[0],
        conditions:
            indexOfOrderBy > 0
                ? parseConditions(
                      slicedCommandAsStringArray.slice(1, indexOfOrderBy)
                  )
                : parseConditions(
                      slicedCommandAsStringArray.slice(
                          1,
                          slicedCommandAsStringArray.length - 1
                      )
                  ),
    }

    return parsedWherePart
}

module.exports = { parseWhere }
