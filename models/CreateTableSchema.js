const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Only letters, numbers and underscore allowed',
        }),

    type: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^INTEGER$|^TEXT$/)
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base': 'Invalid type',
        }),

    constraints: Joi.array().items(
        Joi.string()
            .pattern(/[;]/, { invert: true })
            .pattern(/^PRIMARY KEY$/)
            .insensitive()
            .optional()
            .messages({
                'string.pattern.invert.base':
                    'Semicolon should be only found at the end of a query',
                'string.pattern.base': 'Invalid constraint or comma missing',
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
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
        }),

    tableName: Joi.string()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
        }),

    openingBracket: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\($/)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
        }),

    columns: Joi.array().items(ColumnsSchema).required(),

    closingBracket: Joi.boolean().valid(true).required().messages({}),

    finalSemicolon: Joi.string().valid(';').required().messages({
        'string.any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { CreateTableSchema, ColumnsSchema }
