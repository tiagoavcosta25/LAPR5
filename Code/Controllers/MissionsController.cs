using DDDNetCore.Domain.Missions;
using DDDNetCore.Domain.Missions.DTOS;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class MissionsController : ControllerBase
    {
        private readonly MissionService _service;

        public MissionsController(MissionService service)
        {
            _service = service;
        }

        // GET: api/Missions
        [HttpGet]
        public async Task<ActionResult<IEnumerable<MissionDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Missions/5
        [HttpGet("{id}")]
        public async Task<ActionResult<MissionDto>> GetGetById(Guid id)
        {
            var mis = await _service.GetByIdAsync(new MissionId(id));

            if (mis == null)
            {
                return NotFound();
            }

            return mis;
        }

        // POST: api/Missions
        [HttpPost]
        public async Task<ActionResult<MissionDto>> Create(CreatingMissionDto dto)
        {
            try
            {
                var mis = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = mis.Id }, mis);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PUT: api/Missions/5
        [HttpPut("{id}")]
        public async Task<ActionResult<MissionDto>> Update(string id, MissionDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var mis = await _service.UpdateAsync(dto);

                if (mis == null)
                {
                    return NotFound();
                }
                return Ok(mis);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // Inactivate: api/Missions/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<MissionDto>> SoftDelete(Guid id)
        {
            var mis = await _service.InactivateAsync(new MissionId(id));

            if (mis == null)
            {
                return NotFound();
            }

            return Ok(mis);
        }

        // DELETE: api/Connections/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<MissionDto>> HardDelete(Guid id)
        {
            try
            {
                var mis = await _service.DeleteAsync(new MissionId(id));

                if (mis == null)
                {
                    return NotFound();
                }

                return Ok(mis);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // CRUD OVER //

    }
}
