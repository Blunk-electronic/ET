------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GENERAL DECLARATIONS                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.directories;

package body et_general is

	
	function directory_entries (
	-- Returns the entries of the given directory. Parameter category determines whether to
	-- search for directories, ordinary files or special files.
		target_directory	: in string;						-- ../lbr
		category			: in ada.directories.file_kind;		-- directory, ordinary_file, special_file
		pattern				: in string) 						-- *.txt
		return type_directory_entries.list is

		use ada.directories;
		filter : filter_type;
		entries : type_directory_entries.list; -- to be returned
		
		procedure do_it (item : in directory_entry_type) is
		-- appends items to the container "entries"
		begin
			type_directory_entries.append (
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


	function to_paper_size (paper_size : in string) return type_paper_size is
	-- converts a string to type_paper_size
	begin
		return type_paper_size'value (paper_size);
	end to_paper_size;
	
	function to_string (paper_size : in type_paper_size) return string is
	begin
		return type_paper_size'image (paper_size);
	end to_string;
	
	-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push(x : item) is
		begin
			top := top + 1;
			s(top) := x;
		end push;

		function pop return item is
		begin
			top := top - 1;
			return s(top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;
	
	end stack_lifo;

end et_general;

-- Soli Deo Gloria
