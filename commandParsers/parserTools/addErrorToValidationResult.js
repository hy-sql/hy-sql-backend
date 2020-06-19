const SQLError = require('../../models/SQLError')

/**
 * If the length of the given command array exceeds the given expected length
 * an error message is created and added into the given validation object. The created
 * error contains the additonal part starting at the expected length and ending at
 * fullCommandAsStringArray[fullCommandAsStringArray.length - 2].
 * @param {string[]} fullCommandAsStringArray full command as array
 * @param {Object} validatedCommand Joi validation result object
 * @param {Number} expectedLength expected length
 * @returns {Object} Joi validation result object
 */
const checkForAdditionalAtEnd = (
    fullCommandAsStringArray,
    validatedCommand,
    expectedLength
) => {
    if (fullCommandAsStringArray.length > expectedLength) {
        const additional = fullCommandAsStringArray
            .slice(expectedLength - 1, fullCommandAsStringArray.length - 1)
            .join(' ')

        throw new SQLError(
            `The following part of the query is probably incorrect and causing it to fail: '${additional}'`
        )
    }

    return validatedCommand
}

module.exports = { checkForAdditionalAtEnd }
