const SQLError = require('../../models/SQLError')

/**
 * If the length of the given command array exceeds the given expected length
 * an error is thrown. The created error contains the additonal part starting
 * at the expected length and ending at
 * fullCommandAsStringArray[fullCommandAsStringArray.length - 2].
 * @param {string[]} fullCommandAsStringArray full command as array
 * @param {Number} expectedLength expected length
 * @returns {Object} Joi validation result object
 */
const checkForAdditionalAtEnd = (fullCommandAsStringArray, expectedLength) => {
    if (fullCommandAsStringArray.length > expectedLength) {
        const additional = fullCommandAsStringArray
            .slice(expectedLength - 1, fullCommandAsStringArray.length - 1)
            .join(' ')

        throw new SQLError(
            `The following part of the query is probably incorrect and causing it to fail: '${additional}'`
        )
    }
}

module.exports = { checkForAdditionalAtEnd }
