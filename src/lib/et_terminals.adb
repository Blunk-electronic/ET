------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         PACKAGE TERMINAL LISTS                           --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;

with et_string_processing;		use et_string_processing;


package body et_terminals is


	function get_terminal_name (
		terminal_cursor	: in pac_terminals.cursor)
		return type_terminal_name
	is (key (terminal_cursor));



	function get_terminal_name (
		terminal_cursor	: in pac_terminals.cursor)
		return string
	is (to_string (get_terminal_name (terminal_cursor)));





	function get_technology (
		terminal_cursor	: in pac_terminals.cursor)
		return type_assembly_technology
	is
		use pac_terminals;
	begin
		return element (terminal_cursor).technology;
	end get_technology;



	procedure iterate (
		terminals	: in pac_terminals.map;
		process		: not null access procedure (position : pac_terminals.cursor);
		proceed		: not null access boolean)
	is
		use pac_terminals;
		c : pac_terminals.cursor := terminals.first;
	begin
		while c /= pac_terminals.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	procedure remove_terminals (
		terminals		: in out pac_terminals.map;
		to_be_removed	: in pac_terminal_names.list)
	is
		use pac_terminal_names;


		procedure query_name (name : in pac_terminal_names.cursor) is
			use pac_terminals;
			--t_cursor : pac_terminals.cursor := find (terminals, element (name));
		begin
			--if t_cursor /= pac_terminals.no_element then
				--delete (terminals, t_cursor);
				---- exclude (terminals, t
			--else
				--null; -- CS warning ?
			--end if;

			-- Delete the terminal indicated by "name" in list "terminals".
			-- The terminal indicated by "name" should exist in
			-- list "terminals":
			delete (terminals, element (name));

		exception
			when constraint_error =>
				--raise semantic_error_1 with "Terminal "
				--& enclose_in_quotes (to_string (element (name)))
				--& " not found !";

				put_line ("WARNING: Terminal "
					& enclose_in_quotes (to_string (element (name)))
					& " not found !");

				-- CS better a log message ?

		end query_name;


	begin
		to_be_removed.iterate (query_name'access);
		null;
	end remove_terminals;



end et_terminals;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
