/* eslint-disable no-unused-vars */
const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'INSERT INTO'

const execute = (fullCommandAsStringList) => {
    console.log('INSERT INTO')
}

module.exports = { isCommand, execute }
