const Joi = require('@hapi/joi')

const CommandSchema = Joi.array().items(
    Joi.string()
        .pattern(/^CREATE TABLE\s|^INSERT INTO\s/, { name: 'commands' })
        .pattern(/.*?(?=[;]+$)/i)
        .required()
        .messages({
            'string.pattern.name':
                'Query was not recognised as any existing valid query',
            'string.pattern.base':
                'Semicolon must always be at the end of an sql expression',
        })
)

module.exports = CommandSchema
