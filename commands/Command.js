/* eslint-disable indent */
/* eslint-disable no-unused-vars */
const CreateTableCommand = require('./CreateTableCommand')
const InsertIntoCommand = require('./InsertIntoCommand')
const SelectAllCommand = require('./SelectAllCommand')

const Command = (input) => {
  const fullCommandAsStringList = input
    .trim()
    .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

  console.log(fullCommandAsStringList)

  switch (true) {
    case fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE':
      return CreateTableCommand(fullCommandAsStringList)

    case fullCommandAsStringList.slice(0, 2).join(' ') === 'INSERT INTO':
      return InsertIntoCommand(fullCommandAsStringList)

    case fullCommandAsStringList.slice(0, 2).join(' ') === 'SELECT *':
      return SelectAllCommand(fullCommandAsStringList)

    default:
      return null
  }
}

module.exports = Command
