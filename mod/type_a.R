# Server for a type A input set

mod_typeA <- function(id, inputs) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      # this is the server for type A set of inputs. It will output the UI, taking
      # into consideration:
      #   - whether this input set has been made before
      #   - whether the type of this input has changed since it was previously made
      #   - what the type of the input is now
      #   
      # Therefore, it needs to have stored somewhere a full input set for all possible
      # types. This can start with some default values, which are then updated when
      # the user updates them
      
    }
  )
}