/* Spaceship
 * Author: Acer
 * Creation date: 11/11/2021
 */

MACHINE Spaceship

    SEES Space

    SETS
        OUTCOME = { movement_successful , movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary , movement_failed_due_to_insufficient_power , collided_into_an_asteroid , movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region , movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon } ;
        MOVEMENT_TYPE = { move_up , move_down , move_forward , move_backward , warp_drive } ;
        DOCKED_STATUS = { yes , no } ;
        GAME_STATUS = { Game_WON , Game_LOST , Game_Not_Over }

    CONCRETE_CONSTANTS
        startingPower ,
        normalMovePowerConsumption ,
        warpDrivePowerConsumption ,
        powerDecrementDueToAsteroidCollision

    PROPERTIES
        startingPower = 100 &  // assumption: Spaceship has 100 units of power when first starting the game
        normalMovePowerConsumption = 5 &
        warpDrivePowerConsumption = 20 &
        powerDecrementDueToAsteroidCollision = 10

    DEFINITIONS
        hasSufficientPower ( currentPower , requiredPower ) == ( currentPower >= requiredPower ) ;
        isInsideSpaceBoundary ( xx , yy ) == ( xx |-> yy : space ) ;
        hasCollidedWithAsteroid ( xx , yy ) == ( xx |-> yy : asteroids ) ;
        isDockedAtStarbase == ( x_coordinate |-> y_coordinate = starbase ) ;
        isGameOver == isDockedAtStarbase or ( power < normalMovePowerConsumption )

    ABSTRACT_VARIABLES
        power ,
        x_coordinate ,
        y_coordinate ,
        collisionsCount ,
        visitedRegions

    INVARIANT
        power : NAT &
        x_coordinate : space_x_axis &
        y_coordinate : space_y_axis &
        x_coordinate |-> y_coordinate : freeSpace &
        collisionsCount : NAT &
        visitedRegions : seq ( freeSpace )

    INITIALISATION
        power := startingPower ||
        x_coordinate := prj1 ( space_x_axis , space_y_axis ) ( homebase ) ||
        y_coordinate := prj2 ( space_x_axis , space_y_axis ) ( homebase ) ||
        collisionsCount := 0 ||
        visitedRegions := [ homebase ]

    OPERATIONS
        // The normal movement of moving up ('MoveUp' functionality)
        outcome , movementType <-- MoveUp =
            PRE
                outcome : OUTCOME & movementType : MOVEMENT_TYPE & not ( isGameOver )
            THEN
                IF ( hasSufficientPower ( power , normalMovePowerConsumption ) )
                THEN
                    IF ( isInsideSpaceBoundary ( x_coordinate , y_coordinate + 1 ) )
                    THEN
                        IF ( not ( hasCollidedWithAsteroid ( x_coordinate , y_coordinate + 1 ) ) )
                        THEN
                            power := power - normalMovePowerConsumption ||
                            y_coordinate := y_coordinate + 1 ||
                            outcome := movement_successful ||
                            movementType := move_up ||
                            visitedRegions := visitedRegions <- ( x_coordinate |-> y_coordinate + 1 )
                        ELSE
                            IF ( power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) >= 0 )
                            THEN
                                power := power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_up
                            ELSE
                                // The scenario where the hitting with an asteroid results decreasing the power of the spaceship below 0. However, power is kept at 0 without making it a minus value 
                                power := 0 ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_up
                            END
                        END
                    ELSE
                        outcome := movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary ||
                        movementType := move_up
                    END
                ELSE
                    outcome := movement_failed_due_to_insufficient_power ||
                    movementType := move_up
                END
            END ;

        // The normal movement of moving down ('MoveDown' functionality)
        outcome , movementType <-- MoveDown =
            PRE
                outcome : OUTCOME & movementType : MOVEMENT_TYPE & not ( isGameOver )
            THEN
                IF ( hasSufficientPower ( power , normalMovePowerConsumption ) )
                THEN
                    IF ( isInsideSpaceBoundary ( x_coordinate , y_coordinate - 1 ) )
                    THEN
                        IF ( not ( hasCollidedWithAsteroid ( x_coordinate , y_coordinate - 1 ) ) )
                        THEN
                            power := power - normalMovePowerConsumption ||
                            y_coordinate := y_coordinate - 1 ||
                            outcome := movement_successful ||
                            movementType := move_down ||
                            visitedRegions := visitedRegions <- ( x_coordinate |-> y_coordinate - 1 )
                        ELSE
                            IF ( power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) >= 0 )
                            THEN
                                power := power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_down
                            ELSE
                                // The scenario where the hitting with an asteroid results decreasing the power of the spaceship below 0. However, power is kept at 0 without making it a minus value
                                power := 0 ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_down
                            END
                        END
                    ELSE
                        outcome := movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary ||
                        movementType := move_down
                    END
                ELSE
                    outcome := movement_failed_due_to_insufficient_power ||
                    movementType := move_down
                END
            END ;

        // The normal movement of moving forward/right ('MoveForward' functionality)
        outcome , movementType <-- MoveForward =
            PRE
                outcome : OUTCOME & movementType : MOVEMENT_TYPE & not ( isGameOver )
            THEN
                IF ( hasSufficientPower ( power , normalMovePowerConsumption ) )
                THEN
                    IF ( isInsideSpaceBoundary ( x_coordinate + 1 , y_coordinate ) )
                    THEN
                        IF ( not ( hasCollidedWithAsteroid ( x_coordinate + 1 , y_coordinate ) ) )
                        THEN
                            power := power - normalMovePowerConsumption ||
                            x_coordinate := x_coordinate + 1 ||
                            outcome := movement_successful ||
                            movementType := move_forward ||
                            visitedRegions := visitedRegions <- ( x_coordinate + 1 |-> y_coordinate )
                        ELSE
                            IF ( power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) >= 0 )
                            THEN
                                power := power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_forward
                            ELSE
                                // The scenario where the hitting with an asteroid results decreasing the power of the spaceship below 0. However, power is kept at 0 without making it a minus value
                                power := 0 ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_forward
                            END
                        END
                    ELSE
                        outcome := movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary ||
                        movementType := move_forward
                    END
                ELSE
                    outcome := movement_failed_due_to_insufficient_power ||
                    movementType := move_forward
                END
            END ;

        // The normal movement of moving backward/left ('MoveBackward' functionality)
        outcome , movementType <-- MoveBackward =
            PRE
                outcome : OUTCOME & movementType : MOVEMENT_TYPE & not ( isGameOver )
            THEN
                IF ( hasSufficientPower ( power , normalMovePowerConsumption ) )
                THEN
                    IF ( isInsideSpaceBoundary ( x_coordinate - 1 , y_coordinate ) )
                    THEN
                        IF ( not ( hasCollidedWithAsteroid ( x_coordinate - 1 , y_coordinate ) ) )
                        THEN
                            power := power - normalMovePowerConsumption ||
                            x_coordinate := x_coordinate - 1 ||
                            outcome := movement_successful ||
                            movementType := move_backward ||
                            visitedRegions := visitedRegions <- ( x_coordinate - 1 |-> y_coordinate )
                        ELSE
                            IF ( power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) >= 0 )
                            THEN
                                power := power - ( normalMovePowerConsumption + powerDecrementDueToAsteroidCollision ) ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_backward
                            ELSE
                                // The scenario where the hitting with an asteroid results decreasing the power of the spaceship below 0. However, power is kept at 0 without making it a minus value
                                power := 0 ||
                                collisionsCount := collisionsCount + 1 ||
                                outcome := collided_into_an_asteroid ||
                                movementType := move_backward
                            END
                        END
                    ELSE
                        outcome := movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary ||
                        movementType := move_backward
                    END
                ELSE
                    outcome := movement_failed_due_to_insufficient_power ||
                    movementType := move_backward
                END
            END ;

        // Warp-drive jump to a given specific place inside the valid space ('EngageWarpDrive' functionality )
        outcome , movementType <-- EngageWarpDrive ( new_x_coordinate , new_y_coordinate ) =
            PRE
                outcome : OUTCOME & movementType : MOVEMENT_TYPE & new_x_coordinate : NAT1 & new_y_coordinate : NAT1 & not ( isGameOver )
            THEN
                IF ( hasSufficientPower ( power , warpDrivePowerConsumption ) )
                THEN
                    IF ( isInsideSpaceBoundary ( new_x_coordinate , new_y_coordinate ) )
                    THEN
                        IF ( new_x_coordinate = x_coordinate & new_y_coordinate = y_coordinate )
                        THEN
                            outcome := movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon ||
                            movementType := warp_drive
                        ELSE
                            IF ( not ( hasCollidedWithAsteroid ( new_x_coordinate , new_y_coordinate ) ) )
                            THEN
                                power := power - warpDrivePowerConsumption ||
                                x_coordinate := new_x_coordinate ||
                                y_coordinate := new_y_coordinate ||
                                outcome := movement_successful ||
                                movementType := warp_drive ||
                                visitedRegions := visitedRegions <- ( new_x_coordinate |-> new_y_coordinate )
                            ELSE
                                outcome := movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region ||
                                movementType := warp_drive
                            END
                        END
                    ELSE
                        outcome := movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary ||
                        movementType := warp_drive
                    END
                ELSE
                    outcome := movement_failed_due_to_insufficient_power ||
                    movementType := warp_drive
                END
            END ;

        // An inquiry operation which reports the current status of the spaceship ('MissionStatus' function)
        currentLocation , currentPower , asteroidCollisions <-- MissionStatus =
            BEGIN
                currentLocation := x_coordinate |-> y_coordinate ||
                currentPower := power ||
                asteroidCollisions := collisionsCount
            END ;

        // An inquiry operation which reports the regions of space that the spaceship has traveled ('RegionsVisited' function)
        route <-- RegionsVisited =
            BEGIN
                route := visitedRegions
            END ;

        // An inquiry operation which reports whether the spaceship is “docked” at the starbase ('DockedAtStarbase' function)
        status <-- DockedAtStarbase =
            PRE
                status : DOCKED_STATUS
            THEN
                IF ( isDockedAtStarbase )
                THEN
                    status := yes
                ELSE
                    status := no
                END
            END ;

        // An inquiry operation which reports the current game status ('GameStatus' function)
        status <-- GameStatus =
            PRE
                status : GAME_STATUS
            THEN
                IF ( isDockedAtStarbase )
                THEN
                    status := Game_WON
                ELSE
                    IF ( power < normalMovePowerConsumption )
                    THEN
                        status := Game_LOST
                    ELSE
                        status := Game_Not_Over
                    END
                END
            END ;

        // Starts or restarts the game with the inserted amount of initial power
        NewGame ( providedPower ) =
            PRE
                providedPower : NAT
            THEN
                power := providedPower || collisionsCount := 0 ||
                x_coordinate := prj1 ( space_x_axis , space_y_axis ) ( homebase ) ||
                y_coordinate := prj2 ( space_x_axis , space_y_axis ) ( homebase ) ||
                visitedRegions := [ homebase ]
            END

END
