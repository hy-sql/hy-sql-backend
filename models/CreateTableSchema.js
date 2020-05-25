const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({}),

    type: Joi.string()
        .valid('INTEGER', 'TEXT')
        .pattern(/[;]./, { invert: true })
        .insensitive()
        .required()
        .messages({}),

    primaryKey: Joi.boolean().required().messages({}),
})

const CreateTableSchema = Joi.object({
    name: Joi.string()
        .valid('CREATE TABLE')
        .pattern(/[;]./, { invert: true })
        .insensitive()
        .required()
        .messages({}),

    tableName: Joi.string().pattern(/^\w+$/).max(64).required().messages({}),

    openingBracket: Joi.boolean().valid(true).required().messages({}),

    columns: Joi.array().items(ColumnsSchema).required(),

    closingBracket: Joi.boolean().valid(true).required().messages({}),

    finalSemicolon: Joi.string().valid(';').optional().messages({}),
})

module.exports = { CreateTableSchema, ColumnsSchema }
