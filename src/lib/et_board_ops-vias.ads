------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / VIAS                            --
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


with ada.containers;   	         	use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with et_vias;						use et_vias;


package et_board_ops.vias is

	
	-- Returns the positions (x/y) of all vias of the given net.
	-- The list of returned points uses fixed point coordinates
	-- as the vias are placed by the operator (their positions are man-made):
	function get_via_positions (
		net_cursor : in pac_nets.cursor)
		return pac_points.list;



	
	-- When a via is to be modified or deleted in the board, then
	-- it must be clearly identified. Since vias have no name a useful
	-- means to identify a via is the associated net:
	type type_proposed_via (category : type_via_category := THROUGH) is record
		via	: type_via (category);
		net	: pac_net_name.bounded_string := no_name; -- GND, CLK
	end record;


	-- When vias are selected among others then we collect them in 
	-- a list:
	package pac_proposed_vias is new indefinite_doubly_linked_lists (type_proposed_via);

	
	-- Returns the position and net name of a proposed via:
	function to_string (
		via	: in pac_proposed_vias.cursor)
		return string;
	
	
	-- Returns all vias in the vicinity of the given point:
	function get_vias (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_vias.list;

	
	
	-- Places a via in the given net:
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level);


	-- Returns the name of the net that is connected
	-- with the given via:
	function get_net (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_via)
		return pac_net_name.bounded_string;
	
	
	-- Moves a via:
	procedure move_via (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_proposed_via;
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	-- Deletes a via:
	procedure delete_via (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_proposed_via; -- incl via and net name
		log_threshold	: in type_log_level);

	
end et_board_ops.vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
