/* if there is something additional between expected end of query and the ending semicolon
  an error about the nonbelonging part is created and added to the given validation object */
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
