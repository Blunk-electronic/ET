------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         PACKAGE TERMINAL LISTS                           --
--                                                                          --
--                              S p e c                                     --
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

with ada.containers;			use ada.containers;
with ada.containers.indefinite_ordered_maps;

with et_logging;				use et_logging;
with et_assembly_technology;	use et_assembly_technology;
with et_terminal_name;			use et_terminal_name;


with et_terminal;				use et_terminal;




package et_terminals is


	package pac_terminals is new indefinite_ordered_maps (
		key_type		=> type_terminal_name, -- H7, 14
		"<"				=> et_terminal_name."<",
		element_type	=> type_terminal);

	use pac_terminals;



	function get_terminal_name (
		terminal_cursor	: in pac_terminals.cursor)
		return type_terminal_name;


	function get_terminal_name (
		terminal_cursor	: in pac_terminals.cursor)
		return string;



	-- Returns the assembly technology of the given terminal:
	function get_technology (
		terminal_cursor	: in pac_terminals.cursor)
		return type_assembly_technology;


	-- Iterates the terminate. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		terminals	: in pac_terminals.map;
		process		: not null access procedure (position : pac_terminals.cursor);
		proceed		: not null access boolean);


	-- Removes from given list "terminals" those given by list "to_be_removed".
	-- Assumes that the terminals given in "to_be_removed" do exist
	-- in "terminals". Otherwise exception is raised:
	procedure remove_terminals (
		terminals		: in out pac_terminals.map;
		to_be_removed	: in pac_terminal_names.list); -- 1, 2, 9, 15


	-- CS
	-- The "negative" of procedure remove_terminals.
	-- Keeps in given list "terminals" those given by list "to_be_kept".
	-- Assumes that the terminals given in "to_be_kept" do exist
	-- in "terminals". Otherwise exception is raised:
	--procedure keep_terminals (
		--terminals	: in out pac_terminals.map;
		--to_be_kept	: in pac_terminal_names.list); -- 1, 2, 9, 15



end et_terminals;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
