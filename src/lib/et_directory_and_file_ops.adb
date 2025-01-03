------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       DIRECTORY AND FILE OPERATIONS                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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


with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with gnat.directory_operations;



package body et_directory_and_file_ops is


	function expand (
		name_in			: in string) -- $HOME/libraries/devices/7400.dev
		--log_threshold	: et_string_processing.type_log_level)
		return string is

		prefix : constant string := ("$"); -- CS: windows ? (like %home%)
		separator : constant string := (1 * gnat.directory_operations.dir_separator); -- /\
		
		place_prefix, place_separator : natural := 0;
		--use gnat.directory_operations;
		--use et_string_processing;
		
-- 		function do_it (path : in string) return string is begin
-- 			log ("full path is " & path, log_threshold + 1);
-- 			return path;
-- 		end;
		
	begin -- expand
		place_prefix := index (name_in, prefix);
		place_separator := index (name_in, separator);

		if place_prefix = 0 then -- no environment variable found
			return name_in; -- return given name as it is
		else
			-- name contains an environment variable
			--log_indentation_up;
			
			--log ("expanding " & name_in, log_threshold);
			-- CS test_vars (name_in); -- test if environment variables exist
			
			--log_indentation_down;
			
			--return do_it (gnat.directory_operations.expand_path (name_in));
			return gnat.directory_operations.expand_path (name_in);
		end if;

	end expand;



	
	function make_filter_pattern (extension : in string) return string is 
		use ada.directories;
	begin
		return compose (name => "*", extension => extension);
	end make_filter_pattern;


	

	function directory_entries (
		target_directory	: in string;						-- ../lbr
		category			: in ada.directories.file_kind;		-- directory, ordinary_file, special_file
		pattern				: in string) 						-- *.txt
		return pac_directory_entries.list is

		use ada.directories;
		filter : filter_type;
		entries : pac_directory_entries.list; -- to be returned
		
		procedure do_it (item : in directory_entry_type) is
		-- appends items to the container "entries"
		begin
			pac_directory_entries.append (
				container => entries,
				new_item => simple_name (item));
		end do_it;

	begin -- directory_entries

		-- set filter according to the item category:
		case category is
			when directory =>
				filter := (directory => true, others => false);

			when ordinary_file =>
				filter := (ordinary_file => true, others => false);

			when special_file =>
				filter := (special_file => true, others => false);
		end case;

		-- start search
		search (target_directory, pattern, filter, do_it'access);
		
		return entries;
	end directory_entries;

	
	
end et_directory_and_file_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
