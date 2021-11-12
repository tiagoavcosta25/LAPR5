using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ConnectionRequestsController : ControllerBase
    {
        private readonly ConnectionRequestService _service;

        public ConnectionRequestsController(ConnectionRequestService service)
        {
            _service = service;
        }

        // GET: api/ConnectionRequests
        [HttpGet]
        public async Task<ActionResult<IEnumerable<ConnectionRequestDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Connections/5
        [HttpGet("{id}")]
        public async Task<ActionResult<ConnectionRequestDto>> GetGetById(Guid id)
        {
            var con = await _service.GetByIdAsync(new ConnectionRequestId(id));

            if (con == null)
            {
                return NotFound();
            }

            return con;
        }

        // POST: api/Connections/dir
        [HttpPost("dir")]
        public async Task<ActionResult<DirectRequestDto>> CreateDir(CreatingDirectRequestDto dto)
        {
            try
            {
                var con = await _service.AddDirAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // POST: api/Connections/intr
        [HttpPost("intr")]
        public async Task<ActionResult<IntroductionRequestDto>> CreateIntr(CreatingIntroductionRequestDto dto)
        {
            try
            {
                var con = await _service.AddIntAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/ConnectionRequests/dir/5
        [HttpPut("dir/{id}")]
        public async Task<ActionResult<DirectRequestDto>> UpdateDir(string id, DirectRequestDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var con = await _service.UpdateDirAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/ConnectionRequests/int/5
        [HttpPut("int/{id}")]
        public async Task<ActionResult<IntroductionRequestDto>> UpdateInt(string id, IntroductionRequestDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var con = await _service.UpdateIntAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

    }
}
