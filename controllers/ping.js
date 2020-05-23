const pingRouter = require('express').Router()

pingRouter.get('/', (req, res) => {
    const json = { value: 'pong' }
    res.json(json)
})

module.exports = pingRouter
