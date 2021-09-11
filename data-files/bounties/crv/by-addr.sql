select * FROM
ethereum.udm_events
where tx_id = lower('0x5c7f3b4a544e0456003dc6af2fa6ecc88dd72b0cf180c67003042bed9f7b7569') ---remove
or tx_id = lower('0xe3648c7d4735a90be3a6d527322e3f331113f5c6023c481f34293e1f76999a40') ---withdraw from gaudge
or tx_id = lower('0xc5d81dd59f7912a4648ce3eba062a1b82a75973d01e78299dec321399b94e975') --add liq
or tx_id = lower('0x2121d208a2634e4675fd0565790a0e181641beb29d15e57a58e3e54e0c6515c2') --swap
or tx_id = lower('0x723a46e2a4422ff9d86e1f7d2697b528f8eeb15601bddc90c75cf9739fae6a8a') --gaudge vote
or tx_id = lower('0x66415332cd11e3b58fad6c053e18bceb32ab71e162714945b1120ae915afe3aa') --- claim crv
