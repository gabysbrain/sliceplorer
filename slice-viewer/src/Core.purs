module App.Core where

import DOM (DOM)
import Signal.Channel (CHANNEL)
import Network.HTTP.Affjax (AJAX)

--type AppEffects = (dom :: DOM, ajax :: AJAX)
type AppEffects = (ajax :: AJAX)

