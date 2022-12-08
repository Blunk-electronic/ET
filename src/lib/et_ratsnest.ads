------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              S p e c                                     --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_conductor_segment.boards;

package et_ratsnest is
	
	use pac_geometry_brd;
	

	type type_airwire is new type_line_fine;
	
	package pac_airwires is new doubly_linked_lists (type_airwire);

	
	function to_airwire (
		line : in et_conductor_segment.boards.type_conductor_line)
		return type_airwire;
	
	
	-- Returns true if the container airwires contains the given
	-- candidate airwire:
	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_airwire)
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

	-- GUI relevant only:
	airwire_line_width : constant type_float_positive := 0.01;


	-- Constructs from a list of nodes a list of airwires.
	-- The airwires are returned as a Shortest-Connection-Network (SCN)
	-- or a spanning-subgraph. 
	--  The given list of nodes must contain at least 2 nodes. Otherwise
	-- no airwire will be returned.
	--  CS: For the moment we assume that no node sits on top of another node,
	-- means each node has a unique x/y position.
	--  The algorithm used here bases on the article:
	-- "Shortest Connection Networks And Some Generalizations"
	-- written by R.C.PRIM, date 1957-05-08
	--  Optionally a list of virtual airwires can be given. If this function
	-- attempts to create an airwire that is already in virtual_airwires then
	-- that particular airwire will be discarded and not appended to the return.
	function make_airwires (
		nodes				: in pac_vectors.list;
		virtual_airwires	: in pac_airwires.list := pac_airwires.empty_list)
		return pac_airwires.list;
	
end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
