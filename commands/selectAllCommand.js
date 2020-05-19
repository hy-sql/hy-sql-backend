/* eslint-disable no-unused-vars */
const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'SELECT *'

const execute = (fullCommandAsStringList) => {
    console.log('SELECT *')
}

module.exports = { isCommand, execute }
