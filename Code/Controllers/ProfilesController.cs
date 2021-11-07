using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Profiles;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ProfilesController : ControllerBase
    {
        private readonly ProfileService _service;

        public ProfilesController(ProfileService service)
        {
            _service = service;
        }

        // GET: api/Profiles
        [HttpGet]
        public async Task<ActionResult<IEnumerable<ProfileDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Profiles/5
        [HttpGet("{id}")]
        public async Task<ActionResult<ProfileDto>> GetGetById(Guid id)
        {
            var prof = await _service.GetByIdAsync(new ProfileId(id));

            if (prof == null)
            {
                return NotFound();
            }

            return prof;
        }

        // POST: api/Profiles
        [HttpPost]
        public async Task<ActionResult<ProfileDto>> Create(CreatingProfileDto dto)
        {
            try
            {
                var prof = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = prof.Id }, prof);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        
        // PUT: api/Profiles/5
        [HttpPut("{id}")]
        public async Task<ActionResult<ProfileDto>> Update(Guid id, ProfileDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var prof = await _service.UpdateAsync(dto);
                
                if (prof == null)
                {
                    return NotFound();
                }
                return Ok(prof);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        // Inactivate: api/Profiles/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ProfileDto>> SoftDelete(Guid id)
        {
            var prof = await _service.InactivateAsync(new ProfileId(id));

            if (prof == null)
            {
                return NotFound();
            }

            return Ok(prof);
        }
        
        // DELETE: api/Profiles/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<ProfileDto>> HardDelete(Guid id)
        {
            try
            {
                var prof = await _service.DeleteAsync(new ProfileId(id));

                if (prof == null)
                {
                    return NotFound();
                }

                return Ok(prof);
            }
            catch(BusinessRuleValidationException ex)
            {
               return BadRequest(new {Message = ex.Message});
            }
        }
    }
}