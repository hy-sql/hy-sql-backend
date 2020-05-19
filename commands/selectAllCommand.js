/* eslint-disable no-unused-vars */
const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'SELECT *'

const execute = (fullCommandAsStringList) => {
    console.log('SELECT *')
    console.log(fullCommandAsStringList)

    // validate list minsize here and return error if too small
    const command = parseCommand(fullCommandAsStringList)

    /* validate here when joi validation completed and if errors return error
   */

    // return command or error
}

const parseCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    console.log(command)

    return command
}

module.exports = { isCommand, execute }
