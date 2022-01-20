using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Players;
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
        private readonly IConnectionRequestService _service;

        public ConnectionRequestsController(IConnectionRequestService service)
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

        // POST: api/ConnectionRequests/dir
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

        // POST: api/ConnectionRequests/intr
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

        // Inactivate: api/ConnectionRequests/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ConnectionRequestDto>> SoftDelete(Guid id)
        {
            var con = await _service.InactivateAsync(new ConnectionRequestId(id));

            if (con == null)
            {
                return NotFound();
            }

            return Ok(con);
        }

        // DELETE: api/ConnectionRequests/5/hard
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<ConnectionRequestDto>> HardDelete(Guid id)
        {
            try
            {
                var con = await _service.DeleteAsync(new ConnectionRequestId(id));

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


        // CRUD OVER //

        // GET: api/ConnectionRequests/pendingRequests/email
        [HttpGet("pendingRequests/{email}")]
        public async Task<ActionResult<IEnumerable<TargetPendingRequestDto>>> GetAllUserPendingDirectRequests(string email)
        {
            try
            {
                var list = await _service.GetAllUserPendingDirectRequestsAsync(email);
                if (list == null)
                {
                    return NotFound();
                }
                return list;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // TODO: Unused method, remove it 
        // GET: api/connectionRequests/pendingRequests/emails?emailPlayer=email1@gmail.com&emailTarget=email2@gmail.com
        [HttpGet("pendingRequests/emails")]
        public async Task<ActionResult<ConnectionRequestDto>> GetByEmails(string emailPlayer, string emailTarget)
        {
            try
            {
                var con = await _service.GetByEmailsAsync(emailPlayer, emailTarget);
                if (con == null)
                {
                    return NotFound();
                }
                return con;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PATCH: api/ConnectionRequests/pendingRequests/id/accept/
        [HttpPatch("pendingRequests/{id}/accept")]
        public async Task<ActionResult<AcceptRequestDto>> AcceptRequest(string id, AcceptRequestDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var conR = await _service.AcceptRequest(dto);

                if (conR == null)
                {
                    return NotFound();
                }

                return Ok(dto);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PATCH: api/ConnectionRequests/pendingRequests/id/deny/
        [HttpPatch("pendingRequests/{id}/deny")]
        public async Task<ActionResult<AcceptRequestDto>> DenyRequest(string id)
        {

            try
            {
                var conR = await _service.DenyRequest(id);

                if (conR == null)
                {
                    return NotFound();
                }

                return Ok(conR);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/ConnectionRequests/pendingRequests/middleman/email
        [HttpGet("pendingRequests/middleman/{email}")]
        public async Task<ActionResult<IEnumerable<ListMidPendingRequestDto>>> GetAllUserPendingMidRequests(string email)
        {
            return await _service.GetAllUserPendingMidRequests(email);
        }

        // POST: api/ConnectionRequests/directRequest/
        [HttpPost("directRequest")]
        public async Task<ActionResult<CreatingDirectRequestAutoDto>> CreateDirectRequest(CreatingDirectRequestAutoDto dto)
        {
            try
            {
                var con = await _service.AddDirectRequestAsync(dto);

                CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
                return dto;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // PATCH: api/ConnectionRequests/approve/5
        [HttpPatch("approve/{id}")]
        public async Task<ActionResult<ApproveRequestDto>> ApproveRequest(string id, ApproveRequestDto dto)
        {
            try
            {
                var conR = await _service.ApproveRequest(new ConnectionRequestId(id), dto);

                if (conR == null)
                {
                    return NotFound();
                }

                return Ok(conR);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/ConnectionRequests/middleManRequests/playerEmail
        [HttpGet("middleManRequests/{playerEmail}")]
        public async Task<ActionResult<IEnumerable<IntroductionRequestDto>>> GetMiddleManRequests(string playerEmail)
        {
            return await _service.GetMiddleManRequests(playerEmail);
        }

        // GET: api/connectionRequests/pending/emails?emailPlayer=email1@gmail.com&emailTarget=email2@gmail.com
        [HttpGet("pending/emails")]
        public async Task<ActionResult<bool>> CheckIfRequestsPendingBetweenUsers(string emailPlayer, string emailTarget)
        {
            return await _service.CheckIfRequestsPendingBetweenUsers(emailPlayer, emailTarget);
        }

    }
}
