------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--		- 
--		- 

with ada.text_io;				use ada.text_io;
--with ada.characters;			use ada.characters;
--with ada.characters.latin_1;
--with ada.characters.handling;	use ada.characters.handling;

--with ada.strings;				use ada.strings;
--with ada.strings.fixed;			use ada.strings.fixed;
--with ada.strings.maps;			use ada.strings.maps;
--with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
--with ada.containers.indefinite_doubly_linked_lists;
--with ada.containers.ordered_maps;
--with ada.containers.indefinite_ordered_maps;
--with ada.containers.ordered_sets;

--with et_general;
--with et_string_processing;		use et_string_processing;
--with et_logging;				use et_logging;

with et_pcb_coordinates;		use et_pcb_coordinates;
--with et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;

package et_ratsnest is
	
	use pac_geometry_brd;
	use pac_geometry_2;
	

	--type type_airwire is new type_line with null record;

	--type type_airwire_node is new type_point with record
		--processed : boolean := false;
	--end record;

	--package pac_airwire_nodes is new doubly_linked_lists (type_airwire_node);

	

	package pac_airwires is new doubly_linked_lists (type_line);

	-- Returns true if the container airwires contains the given
	-- candidate airwire:
	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_line)
		return boolean;
	
	
	-- Iterates the given list of airwires. Aborts the process when the
	-- proceed-flag goes false:
	procedure iterate (
		airwires	: in pac_airwires.list;
		process		: not null access procedure (position : in pac_airwires.cursor);
		proceed		: not null access boolean);

	
	type type_airwires is record
		lines	: pac_airwires.list;
		hidden	: boolean := false;
	end record;

	status_ratsnest_updated : constant string := "ratsnest updated";

	airwire_line_width : constant type_distance_positive := 0.01;


	-- Constructs from a list of nodes a list of airwires.
	-- The airwires are returned as a Shortest-Connection-Network (SCN).
	function make_airwires (
		nodes	: in pac_points.list)
		return pac_airwires.list;
	
end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
