Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Spaceship))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Spaceship))==(Machine(Spaceship));
  Level(Machine(Spaceship))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Spaceship)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Spaceship))==(Space)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Spaceship))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Spaceship))==(?);
  List_Includes(Machine(Spaceship))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Spaceship))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Spaceship))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Spaceship))==(?);
  Context_List_Variables(Machine(Spaceship))==(?);
  Abstract_List_Variables(Machine(Spaceship))==(?);
  Local_List_Variables(Machine(Spaceship))==(visitedRegions,collisionsCount,y_coordinate,x_coordinate,power);
  List_Variables(Machine(Spaceship))==(visitedRegions,collisionsCount,y_coordinate,x_coordinate,power);
  External_List_Variables(Machine(Spaceship))==(visitedRegions,collisionsCount,y_coordinate,x_coordinate,power)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Spaceship))==(?);
  Abstract_List_VisibleVariables(Machine(Spaceship))==(?);
  External_List_VisibleVariables(Machine(Spaceship))==(?);
  Expanded_List_VisibleVariables(Machine(Spaceship))==(?);
  List_VisibleVariables(Machine(Spaceship))==(?);
  Internal_List_VisibleVariables(Machine(Spaceship))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Spaceship))==(btrue);
  Gluing_List_Invariant(Machine(Spaceship))==(btrue);
  Expanded_List_Invariant(Machine(Spaceship))==(btrue);
  Abstract_List_Invariant(Machine(Spaceship))==(btrue);
  Context_List_Invariant(Machine(Spaceship))==(btrue);
  List_Invariant(Machine(Spaceship))==(power: NAT & x_coordinate: space_x_axis & y_coordinate: space_y_axis & x_coordinate|->y_coordinate: freeSpace & collisionsCount: NAT & visitedRegions: seq(freeSpace))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Spaceship))==(btrue);
  Abstract_List_Assertions(Machine(Spaceship))==(btrue);
  Context_List_Assertions(Machine(Spaceship))==(btrue);
  List_Assertions(Machine(Spaceship))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Spaceship))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Spaceship))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Spaceship))==(power,x_coordinate,y_coordinate,collisionsCount,visitedRegions:=startingPower,prj1(space_x_axis,space_y_axis)(homebase),prj2(space_x_axis,space_y_axis)(homebase),0,[homebase]);
  Context_List_Initialisation(Machine(Spaceship))==(skip);
  List_Initialisation(Machine(Spaceship))==(power:=startingPower || x_coordinate:=prj1(space_x_axis,space_y_axis)(homebase) || y_coordinate:=prj2(space_x_axis,space_y_axis)(homebase) || collisionsCount:=0 || visitedRegions:=[homebase])
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Spaceship))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Spaceship),Machine(Space))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Spaceship))==(btrue);
  List_Constraints(Machine(Spaceship))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Spaceship))==(MoveUp,MoveDown,MoveForward,MoveBackward,EngageWarpDrive,MissionStatus,RegionsVisited,DockedAtStarbase,GameStatus,NewGame);
  List_Operations(Machine(Spaceship))==(MoveUp,MoveDown,MoveForward,MoveBackward,EngageWarpDrive,MissionStatus,RegionsVisited,DockedAtStarbase,GameStatus,NewGame)
END
&
THEORY ListInputX IS
  List_Input(Machine(Spaceship),MoveUp)==(?);
  List_Input(Machine(Spaceship),MoveDown)==(?);
  List_Input(Machine(Spaceship),MoveForward)==(?);
  List_Input(Machine(Spaceship),MoveBackward)==(?);
  List_Input(Machine(Spaceship),EngageWarpDrive)==(new_x_coordinate,new_y_coordinate);
  List_Input(Machine(Spaceship),MissionStatus)==(?);
  List_Input(Machine(Spaceship),RegionsVisited)==(?);
  List_Input(Machine(Spaceship),DockedAtStarbase)==(?);
  List_Input(Machine(Spaceship),GameStatus)==(?);
  List_Input(Machine(Spaceship),NewGame)==(providedPower)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Spaceship),MoveUp)==(outcome,movementType);
  List_Output(Machine(Spaceship),MoveDown)==(outcome,movementType);
  List_Output(Machine(Spaceship),MoveForward)==(outcome,movementType);
  List_Output(Machine(Spaceship),MoveBackward)==(outcome,movementType);
  List_Output(Machine(Spaceship),EngageWarpDrive)==(outcome,movementType);
  List_Output(Machine(Spaceship),MissionStatus)==(currentLocation,currentPower,asteroidCollisions);
  List_Output(Machine(Spaceship),RegionsVisited)==(route);
  List_Output(Machine(Spaceship),DockedAtStarbase)==(status);
  List_Output(Machine(Spaceship),GameStatus)==(status);
  List_Output(Machine(Spaceship),NewGame)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Spaceship),MoveUp)==(outcome,movementType <-- MoveUp);
  List_Header(Machine(Spaceship),MoveDown)==(outcome,movementType <-- MoveDown);
  List_Header(Machine(Spaceship),MoveForward)==(outcome,movementType <-- MoveForward);
  List_Header(Machine(Spaceship),MoveBackward)==(outcome,movementType <-- MoveBackward);
  List_Header(Machine(Spaceship),EngageWarpDrive)==(outcome,movementType <-- EngageWarpDrive(new_x_coordinate,new_y_coordinate));
  List_Header(Machine(Spaceship),MissionStatus)==(currentLocation,currentPower,asteroidCollisions <-- MissionStatus);
  List_Header(Machine(Spaceship),RegionsVisited)==(route <-- RegionsVisited);
  List_Header(Machine(Spaceship),DockedAtStarbase)==(status <-- DockedAtStarbase);
  List_Header(Machine(Spaceship),GameStatus)==(status <-- GameStatus);
  List_Header(Machine(Spaceship),NewGame)==(NewGame(providedPower))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Spaceship),MoveUp)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption));
  List_Precondition(Machine(Spaceship),MoveDown)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption));
  List_Precondition(Machine(Spaceship),MoveForward)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption));
  List_Precondition(Machine(Spaceship),MoveBackward)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption));
  List_Precondition(Machine(Spaceship),EngageWarpDrive)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & new_x_coordinate: NAT1 & new_y_coordinate: NAT1 & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption));
  List_Precondition(Machine(Spaceship),MissionStatus)==(btrue);
  List_Precondition(Machine(Spaceship),RegionsVisited)==(btrue);
  List_Precondition(Machine(Spaceship),DockedAtStarbase)==(status: DOCKED_STATUS);
  List_Precondition(Machine(Spaceship),GameStatus)==(status: GAME_STATUS);
  List_Precondition(Machine(Spaceship),NewGame)==(providedPower: NAT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Spaceship),NewGame)==(providedPower: NAT | power,collisionsCount,x_coordinate,y_coordinate,visitedRegions:=providedPower,0,prj1(space_x_axis,space_y_axis)(homebase),prj2(space_x_axis,space_y_axis)(homebase),[homebase]);
  Expanded_List_Substitution(Machine(Spaceship),GameStatus)==(status: GAME_STATUS | x_coordinate|->y_coordinate = starbase ==> status:=Game_WON [] not(x_coordinate|->y_coordinate = starbase) ==> (power<normalMovePowerConsumption ==> status:=Game_LOST [] not(power<normalMovePowerConsumption) ==> status:=Game_Not_Over));
  Expanded_List_Substitution(Machine(Spaceship),DockedAtStarbase)==(status: DOCKED_STATUS | x_coordinate|->y_coordinate = starbase ==> status:=yes [] not(x_coordinate|->y_coordinate = starbase) ==> status:=no);
  Expanded_List_Substitution(Machine(Spaceship),RegionsVisited)==(btrue | route:=visitedRegions);
  Expanded_List_Substitution(Machine(Spaceship),MissionStatus)==(btrue | currentLocation,currentPower,asteroidCollisions:=x_coordinate|->y_coordinate,power,collisionsCount);
  Expanded_List_Substitution(Machine(Spaceship),EngageWarpDrive)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & new_x_coordinate: NAT1 & new_y_coordinate: NAT1 & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption) | power>=warpDrivePowerConsumption ==> (new_x_coordinate|->new_y_coordinate: space ==> (new_x_coordinate = x_coordinate & new_y_coordinate = y_coordinate ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon,warp_drive [] not(new_x_coordinate = x_coordinate & new_y_coordinate = y_coordinate) ==> (not(new_x_coordinate|->new_y_coordinate: asteroids) ==> power,x_coordinate,y_coordinate,outcome,movementType,visitedRegions:=power-warpDrivePowerConsumption,new_x_coordinate,new_y_coordinate,movement_successful,warp_drive,visitedRegions<-(new_x_coordinate|->new_y_coordinate) [] not(not(new_x_coordinate|->new_y_coordinate: asteroids)) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region,warp_drive)) [] not(new_x_coordinate|->new_y_coordinate: space) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,warp_drive) [] not(power>=warpDrivePowerConsumption) ==> outcome,movementType:=movement_failed_due_to_insufficient_power,warp_drive);
  Expanded_List_Substitution(Machine(Spaceship),MoveBackward)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption) | power>=normalMovePowerConsumption ==> (x_coordinate-1|->y_coordinate: space ==> (not(x_coordinate-1|->y_coordinate: asteroids) ==> power,x_coordinate,outcome,movementType,visitedRegions:=power-normalMovePowerConsumption,x_coordinate-1,movement_successful,move_backward,visitedRegions<-(x_coordinate-1|->y_coordinate) [] not(not(x_coordinate-1|->y_coordinate: asteroids)) ==> (power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 ==> power,collisionsCount,outcome,movementType:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision),collisionsCount+1,collided_into_an_asteroid,move_backward [] not(power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0) ==> power,collisionsCount,outcome,movementType:=0,collisionsCount+1,collided_into_an_asteroid,move_backward)) [] not(x_coordinate-1|->y_coordinate: space) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,move_backward) [] not(power>=normalMovePowerConsumption) ==> outcome,movementType:=movement_failed_due_to_insufficient_power,move_backward);
  Expanded_List_Substitution(Machine(Spaceship),MoveForward)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption) | power>=normalMovePowerConsumption ==> (x_coordinate+1|->y_coordinate: space ==> (not(x_coordinate+1|->y_coordinate: asteroids) ==> power,x_coordinate,outcome,movementType,visitedRegions:=power-normalMovePowerConsumption,x_coordinate+1,movement_successful,move_forward,visitedRegions<-(x_coordinate+1|->y_coordinate) [] not(not(x_coordinate+1|->y_coordinate: asteroids)) ==> (power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 ==> power,collisionsCount,outcome,movementType:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision),collisionsCount+1,collided_into_an_asteroid,move_forward [] not(power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0) ==> power,collisionsCount,outcome,movementType:=0,collisionsCount+1,collided_into_an_asteroid,move_forward)) [] not(x_coordinate+1|->y_coordinate: space) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,move_forward) [] not(power>=normalMovePowerConsumption) ==> outcome,movementType:=movement_failed_due_to_insufficient_power,move_forward);
  Expanded_List_Substitution(Machine(Spaceship),MoveDown)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption) | power>=normalMovePowerConsumption ==> (x_coordinate|->y_coordinate-1: space ==> (not(x_coordinate|->y_coordinate-1: asteroids) ==> power,y_coordinate,outcome,movementType,visitedRegions:=power-normalMovePowerConsumption,y_coordinate-1,movement_successful,move_down,visitedRegions<-(x_coordinate|->y_coordinate-1) [] not(not(x_coordinate|->y_coordinate-1: asteroids)) ==> (power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 ==> power,collisionsCount,outcome,movementType:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision),collisionsCount+1,collided_into_an_asteroid,move_down [] not(power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0) ==> power,collisionsCount,outcome,movementType:=0,collisionsCount+1,collided_into_an_asteroid,move_down)) [] not(x_coordinate|->y_coordinate-1: space) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,move_down) [] not(power>=normalMovePowerConsumption) ==> outcome,movementType:=movement_failed_due_to_insufficient_power,move_down);
  Expanded_List_Substitution(Machine(Spaceship),MoveUp)==(outcome: OUTCOME & movementType: MOVEMENT_TYPE & not(x_coordinate|->y_coordinate = starbase or power<normalMovePowerConsumption) | power>=normalMovePowerConsumption ==> (x_coordinate|->y_coordinate+1: space ==> (not(x_coordinate|->y_coordinate+1: asteroids) ==> power,y_coordinate,outcome,movementType,visitedRegions:=power-normalMovePowerConsumption,y_coordinate+1,movement_successful,move_up,visitedRegions<-(x_coordinate|->y_coordinate+1) [] not(not(x_coordinate|->y_coordinate+1: asteroids)) ==> (power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 ==> power,collisionsCount,outcome,movementType:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision),collisionsCount+1,collided_into_an_asteroid,move_up [] not(power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0) ==> power,collisionsCount,outcome,movementType:=0,collisionsCount+1,collided_into_an_asteroid,move_up)) [] not(x_coordinate|->y_coordinate+1: space) ==> outcome,movementType:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,move_up) [] not(power>=normalMovePowerConsumption) ==> outcome,movementType:=movement_failed_due_to_insufficient_power,move_up);
  List_Substitution(Machine(Spaceship),MoveUp)==(IF power>=normalMovePowerConsumption THEN IF x_coordinate|->y_coordinate+1: space THEN IF not(x_coordinate|->y_coordinate+1: asteroids) THEN power:=power-normalMovePowerConsumption || y_coordinate:=y_coordinate+1 || outcome:=movement_successful || movementType:=move_up || visitedRegions:=visitedRegions<-(x_coordinate|->y_coordinate+1) ELSE IF power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 THEN power:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision) || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_up ELSE power:=0 || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_up END END ELSE outcome:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary || movementType:=move_up END ELSE outcome:=movement_failed_due_to_insufficient_power || movementType:=move_up END);
  List_Substitution(Machine(Spaceship),MoveDown)==(IF power>=normalMovePowerConsumption THEN IF x_coordinate|->y_coordinate-1: space THEN IF not(x_coordinate|->y_coordinate-1: asteroids) THEN power:=power-normalMovePowerConsumption || y_coordinate:=y_coordinate-1 || outcome:=movement_successful || movementType:=move_down || visitedRegions:=visitedRegions<-(x_coordinate|->y_coordinate-1) ELSE IF power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 THEN power:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision) || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_down ELSE power:=0 || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_down END END ELSE outcome:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary || movementType:=move_down END ELSE outcome:=movement_failed_due_to_insufficient_power || movementType:=move_down END);
  List_Substitution(Machine(Spaceship),MoveForward)==(IF power>=normalMovePowerConsumption THEN IF x_coordinate+1|->y_coordinate: space THEN IF not(x_coordinate+1|->y_coordinate: asteroids) THEN power:=power-normalMovePowerConsumption || x_coordinate:=x_coordinate+1 || outcome:=movement_successful || movementType:=move_forward || visitedRegions:=visitedRegions<-(x_coordinate+1|->y_coordinate) ELSE IF power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 THEN power:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision) || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_forward ELSE power:=0 || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_forward END END ELSE outcome:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary || movementType:=move_forward END ELSE outcome:=movement_failed_due_to_insufficient_power || movementType:=move_forward END);
  List_Substitution(Machine(Spaceship),MoveBackward)==(IF power>=normalMovePowerConsumption THEN IF x_coordinate-1|->y_coordinate: space THEN IF not(x_coordinate-1|->y_coordinate: asteroids) THEN power:=power-normalMovePowerConsumption || x_coordinate:=x_coordinate-1 || outcome:=movement_successful || movementType:=move_backward || visitedRegions:=visitedRegions<-(x_coordinate-1|->y_coordinate) ELSE IF power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision)>=0 THEN power:=power-(normalMovePowerConsumption+powerDecrementDueToAsteroidCollision) || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_backward ELSE power:=0 || collisionsCount:=collisionsCount+1 || outcome:=collided_into_an_asteroid || movementType:=move_backward END END ELSE outcome:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary || movementType:=move_backward END ELSE outcome:=movement_failed_due_to_insufficient_power || movementType:=move_backward END);
  List_Substitution(Machine(Spaceship),EngageWarpDrive)==(IF power>=warpDrivePowerConsumption THEN IF new_x_coordinate|->new_y_coordinate: space THEN IF new_x_coordinate = x_coordinate & new_y_coordinate = y_coordinate THEN outcome:=movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon || movementType:=warp_drive ELSE IF not(new_x_coordinate|->new_y_coordinate: asteroids) THEN power:=power-warpDrivePowerConsumption || x_coordinate:=new_x_coordinate || y_coordinate:=new_y_coordinate || outcome:=movement_successful || movementType:=warp_drive || visitedRegions:=visitedRegions<-(new_x_coordinate|->new_y_coordinate) ELSE outcome:=movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region || movementType:=warp_drive END END ELSE outcome:=movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary || movementType:=warp_drive END ELSE outcome:=movement_failed_due_to_insufficient_power || movementType:=warp_drive END);
  List_Substitution(Machine(Spaceship),MissionStatus)==(currentLocation:=x_coordinate|->y_coordinate || currentPower:=power || asteroidCollisions:=collisionsCount);
  List_Substitution(Machine(Spaceship),RegionsVisited)==(route:=visitedRegions);
  List_Substitution(Machine(Spaceship),DockedAtStarbase)==(IF x_coordinate|->y_coordinate = starbase THEN status:=yes ELSE status:=no END);
  List_Substitution(Machine(Spaceship),GameStatus)==(IF x_coordinate|->y_coordinate = starbase THEN status:=Game_WON ELSE IF power<normalMovePowerConsumption THEN status:=Game_LOST ELSE status:=Game_Not_Over END END);
  List_Substitution(Machine(Spaceship),NewGame)==(power:=providedPower || collisionsCount:=0 || x_coordinate:=prj1(space_x_axis,space_y_axis)(homebase) || y_coordinate:=prj2(space_x_axis,space_y_axis)(homebase) || visitedRegions:=[homebase])
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Spaceship))==(startingPower,normalMovePowerConsumption,warpDrivePowerConsumption,powerDecrementDueToAsteroidCollision);
  Inherited_List_Constants(Machine(Spaceship))==(?);
  List_Constants(Machine(Spaceship))==(startingPower,normalMovePowerConsumption,warpDrivePowerConsumption,powerDecrementDueToAsteroidCollision)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Spaceship),OUTCOME)==({movement_successful,movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,movement_failed_due_to_insufficient_power,collided_into_an_asteroid,movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region,movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon});
  Context_List_Enumerated(Machine(Spaceship))==(?);
  Context_List_Defered(Machine(Spaceship))==(?);
  Context_List_Sets(Machine(Spaceship))==(?);
  List_Valuable_Sets(Machine(Spaceship))==(?);
  Inherited_List_Enumerated(Machine(Spaceship))==(?);
  Inherited_List_Defered(Machine(Spaceship))==(?);
  Inherited_List_Sets(Machine(Spaceship))==(?);
  List_Enumerated(Machine(Spaceship))==(OUTCOME,MOVEMENT_TYPE,DOCKED_STATUS,GAME_STATUS);
  List_Defered(Machine(Spaceship))==(?);
  List_Sets(Machine(Spaceship))==(OUTCOME,MOVEMENT_TYPE,DOCKED_STATUS,GAME_STATUS);
  Set_Definition(Machine(Spaceship),MOVEMENT_TYPE)==({move_up,move_down,move_forward,move_backward,warp_drive});
  Set_Definition(Machine(Spaceship),DOCKED_STATUS)==({yes,no});
  Set_Definition(Machine(Spaceship),GAME_STATUS)==({Game_WON,Game_LOST,Game_Not_Over})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Spaceship))==(?);
  Expanded_List_HiddenConstants(Machine(Spaceship))==(?);
  List_HiddenConstants(Machine(Spaceship))==(?);
  External_List_HiddenConstants(Machine(Spaceship))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Spaceship))==(btrue);
  Context_List_Properties(Machine(Spaceship))==(space_x_axis = 1..12 & space_y_axis = 1..7 & space = space_x_axis*space_y_axis & asteroids <: space & asteroids = {3|->2,3|->5,5|->4,6|->7,7|->1,7|->5,7|->7,8|->3,10|->6,11|->2,12|->5} & freeSpace <: space & freeSpace/\asteroids = {} & freeSpace\/asteroids = space & starbase: freeSpace & starbase = 6|->4 & homebase: freeSpace & homebase = 1|->1);
  Inherited_List_Properties(Machine(Spaceship))==(btrue);
  List_Properties(Machine(Spaceship))==(startingPower = 100 & normalMovePowerConsumption = 5 & warpDrivePowerConsumption = 20 & powerDecrementDueToAsteroidCollision = 10 & OUTCOME: FIN(INTEGER) & not(OUTCOME = {}) & MOVEMENT_TYPE: FIN(INTEGER) & not(MOVEMENT_TYPE = {}) & DOCKED_STATUS: FIN(INTEGER) & not(DOCKED_STATUS = {}) & GAME_STATUS: FIN(INTEGER) & not(GAME_STATUS = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Spaceship),Machine(Space))==(?);
  Seen_Context_List_Enumerated(Machine(Spaceship))==(?);
  Seen_Context_List_Invariant(Machine(Spaceship))==(btrue);
  Seen_Context_List_Assertions(Machine(Spaceship))==(btrue);
  Seen_Context_List_Properties(Machine(Spaceship))==(btrue);
  Seen_List_Constraints(Machine(Spaceship))==(btrue);
  Seen_List_Operations(Machine(Spaceship),Machine(Space))==(?);
  Seen_Expanded_List_Invariant(Machine(Spaceship),Machine(Space))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Spaceship),MoveUp)==(?);
  List_ANY_Var(Machine(Spaceship),MoveDown)==(?);
  List_ANY_Var(Machine(Spaceship),MoveForward)==(?);
  List_ANY_Var(Machine(Spaceship),MoveBackward)==(?);
  List_ANY_Var(Machine(Spaceship),EngageWarpDrive)==(?);
  List_ANY_Var(Machine(Spaceship),MissionStatus)==(?);
  List_ANY_Var(Machine(Spaceship),RegionsVisited)==(?);
  List_ANY_Var(Machine(Spaceship),DockedAtStarbase)==(?);
  List_ANY_Var(Machine(Spaceship),GameStatus)==(?);
  List_ANY_Var(Machine(Spaceship),NewGame)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Spaceship)) == (startingPower,normalMovePowerConsumption,warpDrivePowerConsumption,powerDecrementDueToAsteroidCollision,OUTCOME,MOVEMENT_TYPE,DOCKED_STATUS,GAME_STATUS,movement_successful,movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary,movement_failed_due_to_insufficient_power,collided_into_an_asteroid,movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region,movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon,move_up,move_down,move_forward,move_backward,warp_drive,yes,no,Game_WON,Game_LOST,Game_Not_Over | ? | visitedRegions,collisionsCount,y_coordinate,x_coordinate,power | ? | MoveUp,MoveDown,MoveForward,MoveBackward,EngageWarpDrive,MissionStatus,RegionsVisited,DockedAtStarbase,GameStatus,NewGame | ? | seen(Machine(Space)) | ? | Spaceship);
  List_Of_HiddenCst_Ids(Machine(Spaceship)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Spaceship)) == (startingPower,normalMovePowerConsumption,warpDrivePowerConsumption,powerDecrementDueToAsteroidCollision);
  List_Of_VisibleVar_Ids(Machine(Spaceship)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Spaceship)) == (?: ?);
  List_Of_Ids(Machine(Space)) == (space_x_axis,space_y_axis,space,freeSpace,asteroids,homebase,starbase | ? | ? | ? | ? | ? | ? | ? | Space);
  List_Of_HiddenCst_Ids(Machine(Space)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Space)) == (space_x_axis,space_y_axis,space,freeSpace,asteroids,homebase,starbase);
  List_Of_VisibleVar_Ids(Machine(Space)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Space)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Spaceship)) == (Type(OUTCOME) == Cst(SetOf(etype(OUTCOME,0,5)));Type(MOVEMENT_TYPE) == Cst(SetOf(etype(MOVEMENT_TYPE,0,4)));Type(DOCKED_STATUS) == Cst(SetOf(etype(DOCKED_STATUS,0,1)));Type(GAME_STATUS) == Cst(SetOf(etype(GAME_STATUS,0,2))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Spaceship)) == (Type(movement_successful) == Cst(etype(OUTCOME,0,5));Type(movement_failed_due_to_the_attempt_of_moving_out_of_the_space_boundary) == Cst(etype(OUTCOME,0,5));Type(movement_failed_due_to_insufficient_power) == Cst(etype(OUTCOME,0,5));Type(collided_into_an_asteroid) == Cst(etype(OUTCOME,0,5));Type(movement_failed_due_to_the_attempt_of_warp_driving_into_an_asteroid_region) == Cst(etype(OUTCOME,0,5));Type(movement_failed_due_to_the_attempt_of_warp_driving_into_the_current_reigon) == Cst(etype(OUTCOME,0,5));Type(move_up) == Cst(etype(MOVEMENT_TYPE,0,4));Type(move_down) == Cst(etype(MOVEMENT_TYPE,0,4));Type(move_forward) == Cst(etype(MOVEMENT_TYPE,0,4));Type(move_backward) == Cst(etype(MOVEMENT_TYPE,0,4));Type(warp_drive) == Cst(etype(MOVEMENT_TYPE,0,4));Type(yes) == Cst(etype(DOCKED_STATUS,0,1));Type(no) == Cst(etype(DOCKED_STATUS,0,1));Type(Game_WON) == Cst(etype(GAME_STATUS,0,2));Type(Game_LOST) == Cst(etype(GAME_STATUS,0,2));Type(Game_Not_Over) == Cst(etype(GAME_STATUS,0,2));Type(startingPower) == Cst(btype(INTEGER,?,?));Type(normalMovePowerConsumption) == Cst(btype(INTEGER,?,?));Type(warpDrivePowerConsumption) == Cst(btype(INTEGER,?,?));Type(powerDecrementDueToAsteroidCollision) == Cst(btype(INTEGER,?,?)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Spaceship)) == (Type(visitedRegions) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(collisionsCount) == Mvl(btype(INTEGER,?,?));Type(y_coordinate) == Mvl(btype(INTEGER,?,?));Type(x_coordinate) == Mvl(btype(INTEGER,?,?));Type(power) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Spaceship)) == (Type(NewGame) == Cst(No_type,btype(INTEGER,?,?));Type(GameStatus) == Cst(etype(GAME_STATUS,?,?),No_type);Type(DockedAtStarbase) == Cst(etype(DOCKED_STATUS,?,?),No_type);Type(RegionsVisited) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(MissionStatus) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(EngageWarpDrive) == Cst(etype(OUTCOME,?,?)*etype(MOVEMENT_TYPE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MoveBackward) == Cst(etype(OUTCOME,?,?)*etype(MOVEMENT_TYPE,?,?),No_type);Type(MoveForward) == Cst(etype(OUTCOME,?,?)*etype(MOVEMENT_TYPE,?,?),No_type);Type(MoveDown) == Cst(etype(OUTCOME,?,?)*etype(MOVEMENT_TYPE,?,?),No_type);Type(MoveUp) == Cst(etype(OUTCOME,?,?)*etype(MOVEMENT_TYPE,?,?),No_type));
  Observers(Machine(Spaceship)) == (Type(GameStatus) == Cst(etype(GAME_STATUS,?,?),No_type);Type(DockedAtStarbase) == Cst(etype(DOCKED_STATUS,?,?),No_type);Type(RegionsVisited) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(MissionStatus) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
