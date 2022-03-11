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

        Just (User "Alice") ->
            True

        Just (User "Bob") ->
            True

        Just (User "Charlie") ->
            True

        Just name ->
            False


assignTicketTo : User -> Ticket -> Ticket
assignTicketTo u (Ticket t) =
    case t.status of
        New ->
            Ticket { t | assignedTo = Just u, status = InProgress }

        Archived ->
            Ticket t

        _ ->
            Ticket { t | assignedTo = Just u }
