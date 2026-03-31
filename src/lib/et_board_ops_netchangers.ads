------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / NETCHANGERS                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- To Do: 
--
--
--

with ada.containers;           			use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with et_board_coordinates;				use et_board_coordinates;
with et_board_geometry;					use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_netchangers;					use et_netchangers;

with et_object_status;					use et_object_status;

with et_coordinates_abs_rel;			use et_coordinates_abs_rel;
with et_logging;						use et_logging;



package et_board_ops_netchangers is

	use pac_generic_modules;
	



	-- Moves the given netchanger. 
	-- It is assumed that the netchanger indicated by index
	-- exists in the module. If the netchanger does not exist,
	-- then an exception is raised.
	procedure move_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y, destination or offset
		log_threshold	: in type_log_level);



	
	

end et_board_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
