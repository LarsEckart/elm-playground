module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


emptyComment : ( User, String ) -> Bool
emptyComment ( _, comment ) =
    String.length comment == 0


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket ticket) =
    List.length (List.filter (isFromAuthor (Tuple.first ticket.createdBy)) ticket.comments)


isFromAuthor : User -> ( User, String ) -> Bool
isFromAuthor author comment =
    Tuple.first comment == author


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket ticket) =
    case ticket.assignedTo of
        Nothing ->
            False

        Just user ->
            True


assignTicketTo : User -> Ticket -> Ticket
assignTicketTo u t =
    t
