------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            CANVAS MESSAGES                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- DESCRIPTION:
--
--	This package provides general things required to set up a canvas. A canvas
--  is also referred to as "view". 
--  Since the canvas is used in various drawings like schematic, pcb-layout, ...
--  it must be instantiated with the package that provides the respective
--  measurement system.


package et_canvas_messages is
	
	status_hint_for_abort	: constant string := " Hit ESC to abort.";
	status_click_left		: constant string := "LEFT click ";
	status_click_right		: constant string := "RIGHT click ";
	status_press_space		: constant string := "press SPACE ";
	status_A				: constant string := "start point";
	status_B				: constant string := "end point";
	status_set_B			: constant string := "to set end point.";
	status_set_A			: constant string := "to set start point.";

	status_next_object_clarification : constant string := 
		"For next object click RIGHT or press page-down.";
		--& " Confirm selection with LEFT click or SPACE key.";


	message_border_reached	: constant string := "Border of drawing area reached !";

end et_canvas_messages;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
