------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / RATSNEST                          --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
--   ToDo: 


with ada.containers;   		         	use ada.containers;
with ada.containers.doubly_linked_lists;

with et_nets;							use et_nets;
with et_net_names;						use et_net_names;
with et_ratsnest;						use et_ratsnest;
with et_logging;						use et_logging;

package et_board_ops.ratsnest is


	use pac_net_name;


	-- Returns the start and end positions (x/y) of all track 
	-- segments (lines and arcs) of the given net:
	-- The list of returned points uses fixed point coordinates
	-- as the tracks are placed by the operator (their ends are man-made):
	function get_track_ends (
		net_cursor : in pac_nets.cursor)
		return pac_points.list;


	
	-- (Re)generates the ratsnest of all nets according to the current
	-- positions of vias, tracks and terminals:
	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level);



	-- When airwires are to be collected in the vicinity of a certain 
	-- point then each airwire can be identified with a net name:
	
	type type_proposed_airwire is record
		wire		: type_airwire;
		net_name	: pac_net_name.bounded_string; -- RESET_N
	end record;

	package pac_proposed_airwires is new doubly_linked_lists (type_proposed_airwire);
	use pac_proposed_airwires;
	

	-- Returns all airwires in the vicinity of the given point:
	function get_airwires (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_airwires.list;

	
end et_board_ops.ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
