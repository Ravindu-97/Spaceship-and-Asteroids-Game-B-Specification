/* Space
 * Author: Acer
 * Creation date: 11/11/2021
 */

MACHINE Space
    
    CONSTANTS
        space_x_axis, 
        space_y_axis, 
        space, 
        freeSpace,
        asteroids,  
        homebase, 
        starbase
        
    PROPERTIES
        // Defining the valid range of space for the game
        space_x_axis = 1..12 &
        space_y_axis = 1..7 &
        space = space_x_axis * space_y_axis &
        
        // Defining the asteroids properties and positions
        asteroids <: space &
        asteroids = {3|->2, 3|->5, 5|->4, 6|->7, 7|->1, 7|->5, 7|->7, 8|->3, 10|->6, 11|->2, 12|->5} &
        
        // Defining the empty space for the game
        freeSpace <: space &
        freeSpace /\ asteroids = {} &
        freeSpace \/ asteroids = space &
        
        // Defining the properties and positions of starbase and homebase 
        starbase : freeSpace & starbase = 6|->4 &
        homebase : freeSpace & homebase = 1|->1 
        
END