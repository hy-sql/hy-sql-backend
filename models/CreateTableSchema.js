const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().alphanum().max(64).required().messages({}),

    type: Joi.string()
        .valid('INTEGER', 'TEXT')
        .insensitive()
        .required()
        .messages({}),

    primaryKey: Joi.boolean().required().messages({}),
})

const CreateTableSchema = Joi.object({
    name: Joi.string()
        .valid('CREATE TABLE')
        .insensitive()
        .required()
        .messages({}),

    tableName: Joi.string().pattern(/^\w+$/).max(64).required().messages({}),

    openingBracket: Joi.string().valid('(').required().messages({}),

    columns: Joi.array().items(ColumnsSchema).required(),

    closingBracket: Joi.string().valid(')').required().messages({}),

    finalSemicolon: Joi.string().valid(';').required().messages({}),
})

module.exports = { CreateTableSchema, ColumnsSchema }
