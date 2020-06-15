/** If the length of the given command array exceeds the given expected length
 *  an error message is created and added into the given validation object. The created
 *  error contains the additonal part starting at the expected length and ending at
 *  fullCommandAsStringArray[fullCommandAsStringArray.length - 2].
 * @param {Array} fullCommandAsStringArray full command as array
 * @param {Object} validationResult Joi validation result object
 * @param {Number} expectedLength expected length
 * @returns {Object} Joi validation result object
 */
const checkForAdditionalAtEnd = (
    fullCommandAsStringArray,
    validationResult,
    expectedLength
) => {
    if (fullCommandAsStringArray.length > expectedLength) {
        const additional = fullCommandAsStringArray
            .slice(expectedLength - 1, fullCommandAsStringArray.length - 1)
            .join(' ')
        const errorMessage = `The following part of the query is probably incorrect and causing it to fail: '${additional}'`

        validationResult.error
            ? validationResult.error.details.push({ message: errorMessage })
            : (validationResult.error = {
                  details: [{ message: errorMessage }],
              })
    }

    return validationResult
}

module.exports = checkForAdditionalAtEnd
