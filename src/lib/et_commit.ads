------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               COMMIT                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2023 Mario Blunk, Blunk electronic          --
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


package et_commit is


	-- The commit stage regards the state of the design
	-- before and after a certain operation:
	type type_commit_stage is (
		PRE,
		POST);
	
	
	subtype type_commit_index_zero_based is natural range 0 .. 100;  
	-- CS increase upper limit
	
	subtype type_commit_index is type_commit_index_zero_based 
		range 1 .. type_commit_index_zero_based'last;

	procedure increment (
		index	: in out type_commit_index_zero_based);

	procedure decrement (
		index	: in out type_commit_index_zero_based);


	
	generic
		type type_item is private;
	package pac_commit is

		type type_commit is record
			item	: type_item;
			-- CS time, message
		end record;

		function "=" (
			left, right : in type_commit)
			return boolean;
		
		function make_commit (
			item	: in type_item)
			return type_commit;
		
	end pac_commit;	
	
end et_commit;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
