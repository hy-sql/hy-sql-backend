const { parseField } = require('./fieldParser')
const { checkLimitPosition } = require('./parserTools/checkCorrectPosition')

/**
 * Handles parsing of the LIMIT part of a command into an LIMIT object
 * from the given string array.
 * @param {sting[]} fullCommandAsStringArray the LIMIT part of a command as string array
 */
const parseLimit = (fullCommandAsStringArray) => {
    const limit = {}

    limit.correctlyPlaced = checkLimitPosition(fullCommandAsStringArray)

    const indexOfLimit = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'LIMIT'
    )
    const slicedCommandAsStringArray = fullCommandAsStringArray.slice(
        indexOfLimit
    )

    limit.keyword = slicedCommandAsStringArray[0].toUpperCase()

    const indexOfOffset = slicedCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'OFFSET'
    )

    if (indexOfOffset !== -1) {
        limit.offset = parseOffset(
            slicedCommandAsStringArray.slice(indexOfOffset)
        )
    }

    limit.field =
        indexOfOffset !== -1
            ? parseField(
                  slicedCommandAsStringArray.slice(1, indexOfOffset).join('')
              )
            : parseField(slicedCommandAsStringArray.slice(1).join(''))

    return limit
}

/**
 * Handles parsing of the OFFSET part of the LIMIT part of a command into an OFFSET object
 * from the given string array.
 * @param {sting[]} slicedCommandAsStringArray the OFFSET part of the LIMIT part of a command as string array
 */
const parseOffset = (slicedCommandAsStringArray) => {
    const offset = {
        keyword: slicedCommandAsStringArray[0].toUpperCase(),
        field: parseField(slicedCommandAsStringArray.slice(1).join('')),
    }

    return offset
}

module.exports = { parseLimit }
