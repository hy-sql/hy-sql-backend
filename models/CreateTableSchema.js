const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
            'string.pattern.base':
                'Only letters, numbers and underscore allowed',
        }),

    type: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^INTEGER$|^TEXT$/)
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
            'string.pattern.base': 'Invalid type',
        }),

    constraints: Joi.array().items(
        Joi.string()
            .pattern(/[;]/, { invert: true })
            .pattern(/^PRIMARY KEY$/)
            .insensitive()
            .optional()
            .messages({
                'string.pattern.invert.base': 'Semicolon not allowed here',
                'string.pattern.base': 'Invalid constraint or colon missing',
            })
    ),
})

const CreateTableSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .valid('CREATE TABLE')
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    tableName: Joi.string()
        .pattern(/[;]./, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    openingBracket: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\($/)
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    columns: Joi.array().items(ColumnsSchema).required(),

    closingBracket: Joi.boolean().valid(true).required().messages({}),

    finalSemicolon: Joi.string().valid(';').required().messages({}),
})

module.exports = { CreateTableSchema, ColumnsSchema }
